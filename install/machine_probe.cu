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

// Data structures to hold the machine characteristics
typedef struct {
  int id;
  cudaDeviceProp deviceProp;
  int *accessiblePeers;
  int numAccessiblePeers;
  int globalMemspace;
  float globalPeakBw;
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

// Helper functions to add and free the memory transfer b/w structs
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

// Timer helper functions
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

// Global state
const int RAMID = 0;
const int PINNEDID = 1;
const int GPUOFFSET = 2;
mem_xfer_bw_t *bws = NULL;
int numBws = 0;
gpu_t *gpus = NULL;
int numDevices = 0;
int *ram_data;
int *pinned_data;
int **dev_datas;
FILE *outfile;
struct timeval *start, *end;
int debug;

// Test the memory transfer bandwidth between the src and all the destination
// devices in the bitmask devs
void test_mem_xfer_bw(int *src_data, int data_size, int srcId, int devs,
                      char *src_name) {
  int curNumDevs = __builtin_popcount(devs);
  add_xfer_bw(&bws, numBws, curNumDevs);
  bws[numBws].srcId = srcId;

  int curDev = 0;
  int i;
  for (i = 0; i < numDevices; ++i) {
    chkError(cudaSetDevice(i), "Couldn't set device");
    cudaStreamSynchronize(0);
  }
  start = gettime();
  for (i = 0; i < numDevices; ++i) {
    if ((1 << i) & devs) {
      chkError(cudaSetDevice(i), "Couldn't set device");
      chkError(cudaMemcpy(dev_datas[i], src_data, data_size,
                          cudaMemcpyHostToDevice),
                "Couldn't copy data from RAM to GPU");
      bws[numBws].dstIds[curDev++] = i + GPUOFFSET;
    }
  }
  for (i = 0; i < numDevices; ++i) {
    chkError(cudaSetDevice(i), "Couldn't set device");
    cudaStreamSynchronize(0);
  }
  end = gettime();
  bws[numBws].bw =
      data_size * curNumDevs / diff_timers(start, end) / (1 << 30);
  numBws++;
  
  if (debug) {
    if (curNumDevs == 1) {
      printf("%s to GPU %d B/W: %f\n",
             src_name, bws[numBws - 1].dstIds[0], bws[numBws - 1].bw);
    } else {
      printf("%s to %d GPUs B/W: %f\n",
             src_name, curNumDevs, bws[numBws - 1].bw);
    }
  }
}

int main(int argc, char **argv) {
  int i, j;

  // Set up program parameters
  // TODO: We assume here that any GPU we're going to use has at least 128MB of
  //       global memory.  This may not actually be the case.  We probably want
  //       to parameterize this so as to scale to any memory size.
  int data_size = (16 << 20) * sizeof(int);
  char *outFilename = "parakeetconf.xml";
  debug = 1;

  // Process command line args
  
  // Open output file
  outfile = fopen(outFilename, "w");
  if (!outfile) {
    printf("Couldn't open output file.\n");
    exit(1);
  }

  // Get number of GPU devices
  chkError(cudaGetDeviceCount(&numDevices), "Couldn't get number of devices");
  if (numDevices > sizeof(int) * 8 - 1) {
    printf("Can't support more than %d devices\n", sizeof(int) * 8 - 1);
    exit(1);
  }
  
  // Create a gpu_t struct for each device
  gpus = (gpu_t*)malloc(numDevices * sizeof(gpu_t));
  
  // Create memspace structs for RAM, pinned RAM and for each device
  memspace_t *memspaces =
      (memspace_t*)malloc((numDevices + GPUOFFSET) * sizeof(memspace_t));
  for (i = 0; i < numDevices + GPUOFFSET; ++i) {
    memspaces[i].id = i;
  }
  
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
  memspaces[RAMID].bytes = (uint64_t)atol(buffer) * 1024;
  if (!memspaces[RAMID].bytes) {
    printf("Unable to convert RAM info to uint64_t.\n");
    exit(1);
  }
  memspaces[PINNEDID].bytes = memspaces[RAMID].bytes;
  pclose(cmdfile);
  
  // Allocate some memory for doing RAM <-> GPU transfers.
  ram_data = (int*)malloc(data_size);
  chkError(cudaMallocHost(&pinned_data, data_size),
           "Couldn't malloc pinned host mem");
  dev_datas = (int**)malloc(numDevices * sizeof(int*));
  
  // For each device, get the properties we're interested in
  for (i = 0; i < numDevices; ++i) {
    // Get device properties
    // TODO: Do we need to store this? Could just re-query every time.
    chkError(cudaGetDeviceProperties(&gpus[i].deviceProp, i),
             "Couldn't get properties for device");
    
    // Take into account that RAM = 0 and PinnedRam = 1
    gpus[i].globalMemspace = i + GPUOFFSET;
    memspaces[i+GPUOFFSET].bytes = gpus[i].deviceProp.totalGlobalMem;
    
    // Store the calculated peak global memory b/w
    // TODO: Assumes that all GPUs use DDR, and so uses a x2 multiplier.
    //       If this ever changes, this won't be accurate.
    gpus[i].globalPeakBw =
        gpus[i].deviceProp.memoryClockRate * 2.0f / 1000000.0f *
        gpus[i].deviceProp.memoryBusWidth / 8.0f;

    if (debug) printf("GPU %d Theoretical Peak Global B/W: %f\n",
                      i, gpus[i].globalPeakBw);
    
    // Allocate some device memory space
    chkError(cudaSetDevice(i), "Couldn't switch GPU devices");
    chkError(cudaMalloc(&dev_datas[i], data_size),
             "Couldn't allocate GPU data");

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
          chkError(cudaMemcpyPeer(dev_datas[i], i, src_data, j, data_size),
                    "Couldn't copy data between peer devices");
          cudaStreamSynchronize(0);
          end = gettime();
          peer_bw = data_size / diff_timers(start, end) / (1 << 30);
          add_xfer_bw(&bws, numBws, 1);
          bws[numBws].srcId = gpus[i].globalMemspace;
          bws[numBws].dstIds[0] = j + GPUOFFSET;
          bws[numBws].bw = peer_bw;
          numBws++;
          
          if (debug) printf("P2P transfer from %d to %d: %f\n", j, i, peer_bw);
          
          chkError(cudaSetDevice(j), "Couldn't switch GPU devices");
          chkError(cudaFree(src_data), "Couldn't free peer GPU data");
          chkError(cudaSetDevice(i), "Couldn't switch GPU devices");
        }
      }
    }
  }
  
  // Test RAM <-> devices B/W for every combination of devices
  int numSets = 1 << numDevices;
  int devs;
  for (devs = 1; devs < numSets; ++devs) {
    // Test RAM <-> GPUs bw
    test_mem_xfer_bw(ram_data, data_size, RAMID, devs, "RAM");
    
    // Test Pinned RAM <-> GPU bw
    test_mem_xfer_bw(pinned_data, data_size, PINNEDID, devs, "Pinned RAM");
  }
  
  // Output XML file with the collected data
  int outLevel = 0;
  fprintf(outfile, "<Machine>\n");
  outLevel++;
  for (i = 0; i < numDevices; ++i) {
    fprintf(outfile, "%*s<GPU>\n", outLevel++, "");
    
    // Print out the contents of the CUDA device properties struct
    cudaDeviceProp curProp = gpus[i].deviceProp;
    fprintf(outfile, "%*s<Id>%d</Id>\n", outLevel, "", i);
    fprintf(outfile, "%*s<DeviceName>%s</DeviceName>\n", outLevel, "",
            curProp.name);
    fprintf(outfile, "%*s<TotalGlobalMemory>%ld</TotalGlobalMemory>\n",
            outLevel, "", curProp.totalGlobalMem);
    fprintf(outfile, "%*s<SharedMemPerSM>%ld</SharedMemPerSM>\n",
            outLevel, "", curProp.sharedMemPerBlock);
    fprintf(outfile, "%*s<RegsPerBlock>%d</RegsPerBlock>\n",
            outLevel, "", curProp.regsPerBlock);
    fprintf(outfile, "%*s<WarpSize>%d</WarpSize>\n",
            outLevel, "", curProp.warpSize);
    fprintf(outfile, "%*s<MemPitch>%d</MemPitch>\n",
            outLevel, "", curProp.memPitch);
    fprintf(outfile, "%*s<MaxThreadsPerBlock>%d</MaxThreadsPerBlock>\n",
            outLevel, "", curProp.maxThreadsPerBlock);
    fprintf(outfile, "%*s<MaxThreadsPerDim>\n", outLevel++, "");
    fprintf(outfile, "%*s<X>%d</X>\n", outLevel, "", curProp.maxThreadsDim[0]);
    fprintf(outfile, "%*s<Y>%d</Y>\n", outLevel, "", curProp.maxThreadsDim[1]);
    fprintf(outfile, "%*s<Z>%d</Z>\n", outLevel, "", curProp.maxThreadsDim[2]);
    fprintf(outfile, "%*s</MaxThreadsPerDim>\n", --outLevel, "");
    fprintf(outfile, "%*s<MaxGridSize>\n", outLevel++, "");
    fprintf(outfile, "%*s<X>%d</X>\n", outLevel, "", curProp.maxGridSize[0]);
    fprintf(outfile, "%*s<Y>%d</Y>\n", outLevel, "", curProp.maxGridSize[1]);
    fprintf(outfile, "%*s<Z>%d</Z>\n", outLevel, "", curProp.maxGridSize[2]);
    fprintf(outfile, "%*s</MaxGridSize>\n", --outLevel, "");
    fprintf(outfile, "%*s<ClockRate>%f</ClockRate>\n",
            outLevel, "", curProp.clockRate / 1024.0f / 1024.0f);
    fprintf(outfile, "%*s<TotalConstMem>%d</TotalConstMem>\n",
            outLevel, "", curProp.totalConstMem);
    
    // Print out the other data
    if (gpus[i].numAccessiblePeers > 0) {
      fprintf(outfile, "%*s<AccessiblePeers>\n", outLevel++, "");
      for (j = 0; j < gpus[i].numAccessiblePeers; ++j) {
        fprintf(outfile, "%*s<AccessiblePeer>%d</AccessiblePeer>\n",
                outLevel, "", gpus[i].accessiblePeers[j]);
      }
      fprintf(outfile, "%*s</AccessiblePeers>\n", --outLevel, "");
    }
    fprintf(outfile, "%*s<GlobalMemspace>%d</GlobalMemspace>\n",
            outLevel, "", gpus[i].globalMemspace);
    fprintf(outfile,
            "%*s<TheoreticalPeakGlobalBW>%f</TheoreticalPeakGlobalBW>\n",
            outLevel, "", gpus[i].globalPeakBw);
    
    fprintf(outfile, "%*s</GPU>\n", --outLevel, "");
  }
  
  // Print out Memspace info
  for (i = 0; i < numDevices + GPUOFFSET; ++i) {
    fprintf(outfile, "%*s<MemSpace>\n", outLevel++, "");
    fprintf(outfile, "%*s<Id>%d</Id>\n", outLevel, "", memspaces[i].id);
    fprintf(outfile, "%*s<Bytes>%ld</Bytes>\n", outLevel, "",
            memspaces[i].bytes);
    fprintf(outfile, "%*s</MemSpace>\n", --outLevel, "");
  }
  
  // Print out Memory Transfer B/W info
  for (i = 0; i < numBws; ++i) {
    fprintf(outfile, "%*s<MemXferBW>\n", outLevel++, "");
    fprintf(outfile, "%*s<SrcId>%d</SrcId>\n", outLevel, "", bws[i].srcId);
    fprintf(outfile, "%*s<DstIds>\n", outLevel++, "");
    for (j = 0; j < bws[i].numDsts; ++j) {
      fprintf(outfile, "%*s<DstId>%d</DstId>\n", outLevel, "",
              bws[i].dstIds[j]);
    }
    fprintf(outfile, "%*s</DstIds>\n", --outLevel, "");
    fprintf(outfile, "%*s<BW>%f</BW>\n", outLevel, "", bws[i].bw);
    fprintf(outfile, "%*s</MemXferBW>\n", --outLevel, "");
  }

  fprintf(outfile, "</Machine>\n");
  fclose(outfile);

  // Free memory and return
  free(ram_data);
  cudaFree(pinned_data);
  for (i = 0; i < numDevices; ++i) {
    free(gpus[i].accessiblePeers);
    cudaFree(dev_datas[i]);
  }
  free(gpus);
  free_xfer_bws(bws, numBws);

  return 0;
}
