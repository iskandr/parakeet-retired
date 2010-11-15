#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void usage(void) {
  printf("Usage:\n"
         "--------"
         "-v n       : set number of vectors to be n\n"
         "-l n       : set length of vectors to be n\n"
         "-o <name>  : output data to file <name>\n"
         );
}

int main(int argc, char **argv) {
  int num_vecs = 10000;
  int vec_len = 512;
  char *filename = "output.txt";
  int i;

  // Process command line
  for (i = 1; i < argc; ++i) {
    if (!strcmp(argv[i], "-v")) {
      if (argc < i + 2) {
        usage();
        exit(-1);
      }
      i += 1;
      num_vecs = atoi(argv[i]);
    } else if (!strcmp(argv[i], "-l")) {
      if (argc < i + 2) {
        usage();
        exit(-1);
      }
      i += 1;
      vec_len = atoi(argv[i]);
    } else if (!strcmp(argv[i], "-o")) {
      if (argc < i + 2) {
        usage();
        exit(-1);
      }
      i += 1;
      filename = argv[i];
    }
  }

  FILE *outfile;
  if ((outfile = fopen(filename, "w")) == NULL) {
    fprintf(stderr, "Error: unable to open output file %s\n", filename);
    exit(1);
  }

  int j, offset;
  char buf[10000];
  float d;
  for (i = 0; i < num_vecs; ++i) {
    offset = 0;
    for (j = 0; j < vec_len - 1; ++j) {
      d = (float)rand();
      if (d < 1.0f) d = 1.0f;
      d = rand() / d;
      offset += sprintf(buf + offset, "%f ", d);
    }
    d = (float)rand();
    if (d < 1.0f) d = 1.0f;
    d = rand() / d;
    offset += sprintf(buf + offset, "%f\n", d);
    buf[offset] = '\0';
    fputs(buf, outfile);
  }

  fclose(outfile);

  return 0;
}
