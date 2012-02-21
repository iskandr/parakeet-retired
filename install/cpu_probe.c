/*
 * Parakeet
 *
 * (c) 2009-2012 Eric Hielscher, Alex Rubinsteyn
 *
 * CPU Probe
 *
 * Utility for detecting main CPU characteristics of the given
 * computer for use in Parakeet's code optimization.
 * 
 * Outputs an XML file with the gathered information for use by the Parakeet
 * runtime.
 */

#define _GNU_SOURCE
#include <sched.h>
#include <stdio.h>
#include <sys/types.h>
#include <unistd.h>

typedef struct {
  int affinity_id;
  int core_id;
  int smt_id;
  int apic_id;
} processor_t;

typedef struct {
  int size_bytes;
  int level;
} cache_t;

#define cpuid(input,eax,ebx,ecx,edx) \
  asm("cpuid": "=a" (eax), "=b" (ebx), "=c" (ecx), "=d" (edx) : "a" (input));

#define cpuidc(input,ic,eax,ebx,ecx,edx) \
  asm("cpuid": "=a" (eax), "=b" (ebx), "=c" (ecx), "=d" (edx) : "a" (input),\
      "c" (ic));

#define print_registers() \
  printf("%08lx %08lx %08lx %08lx\n", eax, ebx, ecx, edx);

/**
 * Extract the features we're interested in from the CPUID information.
 * 
 * Note: for now, this only works on newer Intel x86 chips.
 */
int main() {
  unsigned long leaf_max, eax, ebx, ecx, edx;

  cpuid(0, leaf_max, ebx, ecx, edx);
  int i;
  for (i = 0; i <= leaf_max; ++i) {
    cpuid(i, eax, ebx, ecx, edx);
    printf("%08x %08lx %08lx %08lx %08lx\n",i,eax,ebx,ecx,edx);
  }

  int num_threads = 0;
  int num_cores = 0;
  cpuidc(0x0B, 0x00, eax, ebx, ecx, edx);
  num_threads = 0xFFFF & ebx;
  printf("Number of threads reported by CPU: %d\n", num_threads);
  print_registers();
  
  cpuidc(0x0B, 0x01, eax, ebx, ecx, edx);
  num_cores = 0xFFFF & ebx;
  printf("Number of cores reported by CPU: %d\n", num_cores);
  print_registers();
  
  cpuid(0x01, eax, ebx, ecx, edx);
  printf("Number of logical processors per physical package: %d\n",
         (ebx >> 16) & 0xFF);
  cpuid(0x04, eax, ebx, ecx, edx);
  printf("Number of APIC IDs reserved per package: %d\n",
         (eax >> 26) + 1);
  int num_caches = 0;
  int last_level = 0;
  while (!last_level) {
    cpuidc(0x04, num_caches, eax, ebx, ecx, edx);
    if (eax & 0x0F) {
      printf("Number of threads serviced by this cache: %d\n",
             ((eax >> 14) & 0x0C) + 1);
      print_registers();
      num_caches++;
    } else {
      last_level = 1;
    }
  }
  
  pid_t pid = getpid();
  cpu_set_t cpu_set, test;
  for (i = 0; i < num_cores; ++i) {
    CPU_ZERO(&cpu_set);
    CPU_SET(i, &cpu_set);
    sched_setaffinity(pid, sizeof(cpu_set_t), &cpu_set);
    sched_getaffinity(pid, sizeof(cpu_set_t), &test);
    if (CPU_COUNT(&test) == 1 && CPU_ISSET(i, &test)) {
      printf("\nScheduler affinity for this process: %d\n", i);
    }
    
    cpuidc(0x0B, 0x00, eax, ebx, ecx, edx);
    unsigned long apic_id = edx;
    printf("APIC ID: %ld\n", apic_id);
    
    cpuidc(0x0B, 0x00, eax, ebx, ecx, edx);
    num_threads = 0xFFFF & ebx;
    printf("Number of threads reported by CPU: %d\n", num_threads);
    print_registers();
    unsigned long smt_mask = ~((-1) << (eax & 0xF));
    unsigned long smt_id = apic_id & smt_mask;
    printf("SMT_ID: %ld\n", smt_id);
    
    cpuidc(0x0B, 0x01, eax, ebx, ecx, edx);
    num_cores = 0xFFFF & ebx;
    printf("Number of cores reported by CPU: %d\n", num_cores);
    print_registers();
    unsigned long core_mask = ~((-1) << (eax & 0xF));
    unsigned long core_id = apic_id & core_mask;
    printf("CORE_ID: %ld\n", core_id);
  }
  return 0;
}
