const int chunklen = 32768;
const int minfree = 24000000;

int x_len;
int mypitch;
int free_mem;

int xbytes = x_len * mypitch * sizeof(int);

int stagex = 0;
int texchunksperxfer;
int vecsperchunk;
int *devX;
int *devX2;
int num_chunks = 1;
int rows_per_xfer = x_len;
if (free_mem - xbytes < minfree) {
  stagex = 1;
  texchunksperxfer = (free_mem - minfree) / (chunklen * mypitch * 2);
  cudaMalloc((void**)&devX,
             mypitch * texchunksperxfer * chunklen * sizeof(int));
  cudaMalloc((void**)&devX2,
             mypitch * texchunksperxfer * chunklen * sizeof(int));

  num_iters = safe_div(x_len, chunklen * texchunksperxfer);
} else {
  cudaMalloc((void**)&devX, mypitch * x_len * sizeof(int));
}

int i, j;
int first = 1;
int *tmp;
int rowoffset = 0;
int cur = chunklen;
int numrowstocopy = rows_per_xfer;
cudaStream_t stream[2];
cudaStreamCreate(&stream[0]);
cudaStreamCreate(&stream[1]);
while (num_chunks > 0) {
  // Swap chunks
  if (!first) {
    // Have to make sure the memory transfer is done??
    cudaStreamSynchronize(stream[1]);
    tmp = devX;
    devX = devX2;
    devX2 = tmp;
  } else {
    // Copy down new chunk
    cudaMemcpy2DAsync(devX,
                      x_pitch,
                      X + rowoffset * (x_pitch / 4),
                      vec_len * sizeof(int),
                      vec_len * sizeof(int),
                      rows_per_xfer,
                      cudaMemcpyHostToDevice,
                      stream[0]);
    first = 0;
    num_chunks--;
  }

  // Asynchronously copy to other chunk
  if (num_chunks == 1) numrowstocopy = x_len - (rowoffset + rows_per_xfer);
  if (num_chunks > 0) {
    cudaMemcpy2DAsync(devX2,
                      x_pitch,
                      X + (rowoffset + chunklen) * (x_pitch / 4),
                      vec_len * sizeof(int),
                      vec_len * sizeof(int),
                      numrowstocopy,
                      cudaMemcpyHostToDevice,
                      stream[1]);
    num_chunks--;
  }

  // Asynchronously launch kernel(s)
  for (j = 0; j < texchunksperxfer && rowoffset < x_len; ++j) {
    if (rowoffset + chunklen > x_len) cur = x_len - rowoffset;
    cudaBindTexture2D(0, allDistsLeft2DTex,
                      devX + j * chunklen * (x_pitch / 4),
                      TwoDXDesc, vec_len, cur, x_pitch);

    dim2DGrid = dim3(safe_div(c_len, THREADS_PER_DIM),
                     safe_div(cur, THREADS_PER_DIM));

    all_pairs_dists_kernel<<<dim2DGrid, dim2DBlock, stream[0]>>>
      (cur, c_len, vec_len, devOut + rowoffset * c_len);

    rowoffset += chunklen;
  }
}

cudaStreamDestroy(stream[0]);
cudaStreamDestroy(stream[1]);
