/*
 * Parakeet
 *
 * (c) 2009-2011 Eric Hielscher, Alex Rubinsteyn
 *
 * 
 */

#include <cuda_runtime_api.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void chkError(cudaError_t rslt, char *msg) {
  if (rslt) {
    printf("Error: %s\n", msg);
    exit(1);
  }
}

int main(int argc, char **argv) {
  cudaError_t rslt;

  int numDevices;
  rslt = cudaGetDeviceCount(&numDevices);
  chkError(rslt, "couldn't get number of devices");

  cudaDeviceProp *deviceProps =
    (cudaDeviceProp*)malloc(numDevices * sizeof(cudaDeviceProp));

  int i;
  for (i = 0; i < numDevices; ++i) {
    rslt = cudaGetDeviceProperties(&deviceProps[i], i);
    chkError(rslt, "couldn't get properties for device");

    int canAccessPeer;
    rslt = cudaDeviceCanAccessPeer(&canAccessPeer, i, (i+1)%numDevices);
    chkError(rslt, "couldn't get peer access info");

    printf(
        "PROPERTIES FOR DEVICE %d\n"
        "------------------------\n"
        "\n"
        "Device name: %s\n"
        "Total Global mem: %d\n"
        "Shared mem per block: %d\n"
        "Regs per block: %d\n"
        "Warp size: %d\n"
        "Mem pitch: %d\n"
        "Max threads per block: %d\n"
        "Max threads per dim: [%d,%d,%d]\n"
        "Max Grid size: [%d,%d,%d]\n"
        "Clock rate: %d\n"
        "Total const mem: %d\n"
        "Compute capability: %d.%d\n"
        "Texture alignment: %d\n"
        "Device Overlap: %d\n"
        "Multi Processor Count: %d\n"
        "Kernel Exec Timeout Enabled: %d\n"
        "Integrated: %d\n"
        "Can Map Host Mem: %d\n"
        "Compute mode: %d\n"
        "Max texture 1D: %d\n"
        "Max texture 2D: [%d,%d]\n"
        "Max texture 3D: [%d,%d,%d]\n"
        "Max texture 1D layered: [%d,%d]\n"
        "Max texture 2D layered: [%d,%d,%d]\n"
        "Surface alignment: %d\n"
        "Concurrent kernels: %d\n"
        "ECC Enabled: %d\n"
        "PCI Bus ID: %d\n"
        "PCI Device ID: %d\n"
        "PCI Domain ID: %d\n"
        "TCC Driver: %d\n"
        "Async Engine Count: %d\n"
        "Unified Addressing: %d\n"
        "Memory Clock rate: %d\n"
        "Memory Bus Width: %d\n"
        "L2 Cache Size: %d\n"
        "Max Threads per SM: %d\n"
        "Can access peer: %d\n"
        "\n",
        i,
        deviceProps[i].name,
        deviceProps[i].totalGlobalMem,
        deviceProps[i].sharedMemPerBlock,
        deviceProps[i].regsPerBlock,
        deviceProps[i].warpSize,
        deviceProps[i].memPitch,
        deviceProps[i].maxThreadsPerBlock,
        deviceProps[i].maxThreadsDim[0],
        deviceProps[i].maxThreadsDim[1],
        deviceProps[i].maxThreadsDim[2],
        deviceProps[i].maxGridSize[0],
        deviceProps[i].maxGridSize[1],
        deviceProps[i].maxGridSize[2],
        deviceProps[i].clockRate,
        deviceProps[i].totalConstMem,
        deviceProps[i].major,
        deviceProps[i].minor,
        deviceProps[i].textureAlignment,
        deviceProps[i].deviceOverlap,
        deviceProps[i].multiProcessorCount,
        deviceProps[i].kernelExecTimeoutEnabled,
        deviceProps[i].integrated,
        deviceProps[i].canMapHostMemory,
        deviceProps[i].computeMode,
        deviceProps[i].maxTexture1D,
        deviceProps[i].maxTexture2D[0],
        deviceProps[i].maxTexture2D[1],
        deviceProps[i].maxTexture3D[0],
        deviceProps[i].maxTexture3D[1],
        deviceProps[i].maxTexture3D[2],
        deviceProps[i].maxTexture1DLayered[0],
        deviceProps[i].maxTexture1DLayered[1],
        deviceProps[i].maxTexture2DLayered[0],
        deviceProps[i].maxTexture2DLayered[1],
        deviceProps[i].maxTexture2DLayered[2],
        deviceProps[i].surfaceAlignment,
        deviceProps[i].concurrentKernels,
        deviceProps[i].ECCEnabled,
        deviceProps[i].pciBusID,
        deviceProps[i].pciDeviceID,
        deviceProps[i].pciDomainID,
        deviceProps[i].tccDriver,
        deviceProps[i].asyncEngineCount,
        deviceProps[i].unifiedAddressing,
        deviceProps[i].memoryClockRate,
        deviceProps[i].memoryBusWidth,
        deviceProps[i].l2CacheSize,
        deviceProps[i].maxThreadsPerMultiProcessor,
        canAccessPeer);
  }

  free(deviceProps);

  return 0;
}
