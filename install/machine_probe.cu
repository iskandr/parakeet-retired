/*
 * Parakeet
 *
 * (c) 2009-2011 Eric Hielscher, Alex Rubinsteyn
 *
 * Machine Probe
 *
 * Utility for detecting main architectural characteristics of the given
 * computer for use in Parakeet's code optimization.
 * 
 * Outputs an XML file with the gathered information for use by the Parakeet
 * runtime.
 */

#include <cuda_runtime_api.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>
#include <time.h>

typedef struct {
  int id;
  cudaDeviceProp deviceProp;
  int *accessiblePeers;
  int numAccessiblePeers;
  int globalMemspace;
  float globalBw;
} gpu_t;

typedef struct {
  int id;
  uint64_t bytes;
} memspace_t;

typedef struct {
  int srcId;
  int *dstIds;
  int numDsts;
  float bw;
} mem_xfer_bw_t;

void add_xfer_bw(mem_xfer_bw_t **bws, int numBws, int numDsts) {
  mem_xfer_bw_t *tmp =
      (mem_xfer_bw_t*)malloc((numBws + 1) * sizeof(mem_xfer_bw_t));

  if (*bws) {
    memcpy(tmp, *bws, numBws * sizeof(mem_xfer_bw_t));
    free(*bws);
  }

  *bws = tmp;
  (*bws)[numBws].dstIds = (int*)malloc(numDsts * sizeof(int));
  (*bws)[numBws].numDsts = numDsts;
}

void free_xfer_bws(mem_xfer_bw_t *bws, int numBws) {
  if (!bws) return;
  
  int i;
  for (i = 0; i < numBws; ++i) {
    free(bws[i].dstIds);
  }
  free(bws);
}

void chkError(int rslt, char *msg) {
  if (rslt != 0) {
    printf("%s: %d\n", msg, rslt);
    exit(1);
  }
}

double diff_timers(struct timeval *start, struct timeval *end) {
  double ret;

  if (end->tv_usec < start->tv_usec) {
    int nsec = (start->tv_usec - end->tv_usec) / 1000000 + 1;
    start->tv_usec -= 1000000 * nsec;
    start->tv_sec += nsec;
  }
  if (end->tv_usec - start->tv_usec > 1000000) {
    int nsec = (end->tv_usec - start->tv_usec) / 1000000;
    start->tv_usec += 1000000 * nsec;
    start->tv_sec -= nsec;
  }

  ret = (end->tv_sec - start->tv_sec) +
        (end->tv_usec - start->tv_usec) / 1000000.0;

  free(start);
  free(end);

  return ret;
}

struct timeval *gettime(void) {
  struct timeval *ret = (struct timeval*)(malloc(sizeof(struct timeval)));
  gettimeofday(ret, NULL);
  return ret;
}

// Fills the input array with the given value
__global__
void memFill(int *dev_data, int data_size, int val) {
  int idx = blockDim.x * blockIdx.x + threadIdx.x;
  
  if (idx < data_size) {
    dev_data[idx] = val;
  }
}

// Tests memory read bandwidth
__global__
void memReadBw(int *dev_data, int data_size) {
  int idx = blockDim.x * blockIdx.x + threadIdx.x;
  int step = gridDim.x * blockDim.x;
  int step2 = step + step;
  int step3 = step2 + step;
  
  int d1 = dev_data[idx];
  int d2 = dev_data[idx + step];
  int d3 = dev_data[idx + step2];
  int d4 = dev_data[idx + step3];
  dev_data[idx] = d4;
  dev_data[idx + step] = d3;
  dev_data[idx + step2] = d2;
  dev_data[idx + step3] = d1;
}

// Assumes that the current GPU device is set
float time_ram_to_gpu_xfer(void *dev_data, void *ram_data, int data_size) {
  struct timeval *start, *end;
  float ram_gpu_bw = 0.0f;
  cudaStreamSynchronize(0);
  start = gettime();
  chkError(cudaMemcpy(dev_data, ram_data, data_size, cudaMemcpyHostToDevice),
           "Couldn't copy data from RAM to GPU");
  cudaStreamSynchronize(0);
  end = gettime();
  ram_gpu_bw += diff_timers(start, end);  
  return data_size / ram_gpu_bw / (1 << 30);
}  

int main(int argc, char **argv) {
  const int RAMID = 0;
  const int PINNEDID = 1;
  const int GPUOFFSET = 2;

  struct timeval *start, *end;
  int i, j;

  // Set up program parameters
  // TODO: We assume here that any GPU we're going to use has at least 128MB of
  //       global memory.  This may not actually be the case.  We probably want
  //       to parameterize this so as to scale to any memory size.
  int data_size = (16 << 20) * sizeof(int);
  char *outFilename = "parakeetconf.xml";
  int debug = 1;

  // Process command line args
  
  // Open output file
  FILE *outfile = fopen(outFilename, "w");
  if (!outfile) {
    printf("Couldn't open output file.\n");
    exit(1);
  }

  // Get number of GPU devices
  int numDevices;
  chkError(cudaGetDeviceCount(&numDevices), "Couldn't get number of devices");
  
  // Create a gpu_t struct for each device
  gpu_t *gpus = (gpu_t*)malloc(numDevices * sizeof(gpu_t));
  
  // Create memspace structs for RAM and for each device
  memspace_t *memspaces =
      (memspace_t*)malloc((numDevices + 1) * sizeof(memspace_t));
  for (i = 0; i < numDevices + 1; ++i) {
    memspaces[i].id = i;
  }
  
  // Memory transfer structs for each valid memspace group
  int numBws = 0;
  mem_xfer_bw_t *bws = NULL;
  
  // Set up special RAM memspace
  // TODO: This probably is Ubuntu-specific; need to make it general.
  char *cmd = "awk '{if(NR==1){print $2}}' /proc/meminfo";
  FILE *cmdfile = popen(cmd, "r");
  if (!cmdfile) {
    printf("Unable to get RAM info.\n");
    exit(1);
  }
  char buffer[128];
  memset(buffer, 0, 128);
  if (!fgets(buffer, 128, cmdfile)) {
    printf("Unable to read RAM info from /proc/meminfo.\n");
    exit(1);
  }
  memspaces[RAMID].bytes = (uint64_t)atol(buffer);
  if (!memspaces[RAMID].bytes) {
    printf("Unable to convert RAM info to uint64_t.\n");
    exit(1);
  }
  pclose(cmdfile);
  
  // Allocate some memory for doing RAM <-> GPU transfers.
  int *ram_data = (int*)malloc(data_size);
  int *pinned_data;
  chkError(cudaMallocHost(&pinned_data, data_size),
           "Couldn't malloc pinned host mem");
  
  // For each device, get the properties we're interested in
  for (i = 0; i < numDevices; ++i) {
    // Current memspace ID is i + 1, since RAM is 0
    int curId = i + GPUOFFSET;
    
    // Get device properties
    // TODO: Do we need to store this? Could just re-query every time.
    chkError(cudaGetDeviceProperties(&gpus[i].deviceProp, i),
             "Couldn't get properties for device");
    
    // Allocate some device memory space
    int *dev_data;
    chkError(cudaSetDevice(i), "Couldn't switch GPU devices");
    chkError(cudaMalloc(&dev_data, data_size), "Couldn't allocate GPU data");
    
    // Test GPU <-> Global memory bw
    // 1. Fill the global memory with a value
    // 2. Read that value
    int blockWidth = gpus[i].deviceProp.maxThreadsDim[0] / 4;
    int numBlocks = data_size / sizeof(int) / blockWidth;
    memFill<<<numBlocks, blockWidth>>>(dev_data, data_size / sizeof(int), 1);
    numBlocks /= 4;
    cudaStreamSynchronize(0);
    start = gettime();
    memReadBw<<<numBlocks, blockWidth>>>(dev_data, data_size / sizeof(int));
    cudaStreamSynchronize(0);
    end = gettime();
    float global_mem_bw = data_size / diff_timers(start, end) / (1 << 30);
    
    printf("GPU %d Global Memory BW: %f\n", i, global_mem_bw);
    
    // Test RAM <-> GPU bw
    add_xfer_bw(&bws, numBws, 1);
    bws[numBws].srcId = RAMID;
    bws[numBws].dstIds[0] = curId;
    bws[numBws].bw = time_ram_to_gpu_xfer(dev_data, ram_data, data_size);
    numBws++;
    
    printf("RAM to GPU %d BW: %f\n", i, bws[numBws - 1].bw);
    
    // Test Pinned RAM <-> GPU bw
    add_xfer_bw(&bws, numBws, 1);
    bws[numBws].srcId = PINNEDID;
    bws[numBws].dstIds[0] = curId;
    bws[numBws].bw = time_ram_to_gpu_xfer(dev_data, pinned_data, data_size);
    numBws++;
    
    printf("Pinned RAM to GPU %d BW: %f\n", i, bws[numBws - 1].bw);
    
    // TODO: Test RAM <-> GPU + every other GPU's bw

    // Get peer access info
    gpus[i].numAccessiblePeers = 0;
    int canAccessPeer;
    for (j = 0; j < numDevices; ++j) {
      if (i != j) {
        chkError(cudaDeviceCanAccessPeer(&canAccessPeer, i, j),
                 "Couldn't get peer access info");
        if (canAccessPeer) {
          gpus[i].numAccessiblePeers++;
          chkError(cudaDeviceEnablePeerAccess(j, 0),
                   "Couldn't enable peer access");
        }
      }
    }
    gpus[i].accessiblePeers =
        (int*)malloc(gpus[i].numAccessiblePeers * sizeof(int));
    int cur = 0;
    for (j = 0; j < numDevices; ++j) {
      if (i != j) {
        chkError(cudaDeviceCanAccessPeer(&canAccessPeer, i, j),
                 "Couldn't get peer access info");
        if (canAccessPeer) {
          gpus[i].accessiblePeers[cur++] = j;
          
          // Test P2P memory bandwidth and record 
          chkError(cudaSetDevice(j), "Coudln't switch GPU devices");
          int *src_data;
          chkError(cudaMalloc(&src_data, data_size),
                   "Couldn't allocate peer GPU data");
          chkError(cudaSetDevice(i), "Couldn't switch GPU devices");
          
          float peer_bw = 0.0f;
          cudaStreamSynchronize(0);
          start = gettime();
          chkError(cudaMemcpyPeer(dev_data, i, src_data, j, data_size),
                    "Couldn't copy data between peer devices");
          cudaStreamSynchronize(0);
          end = gettime();
          peer_bw = data_size / diff_timers(start, end) / (1 << 30);
          add_xfer_bw(&bws, numBws, 1);
          bws[numBws].srcId = curId;
          bws[numBws].dstIds[0] = j + GPUOFFSET;
          bws[numBws].bw = peer_bw;
          numBws++;
          
          printf("P2P transfer from %d to %d: %f\n", j, i, peer_bw);
          
          chkError(cudaSetDevice(j), "Couldn't switch GPU devices");
          chkError(cudaFree(src_data), "Couldn't free peer GPU data");
          chkError(cudaSetDevice(i), "Couldn't switch GPU devices");
        }
      }
    }
    
    chkError(cudaFree(dev_data), "Couldn't free GPU data");
  }

  free(ram_data);
  cudaFree(pinned_data);
  for (i = 0; i < numDevices; ++i) {
    free(gpus[i].accessiblePeers);
  }
  free(gpus);
  free_xfer_bws(bws, numBws);

  return 0;
}
