define void @"map_wrapper[array1<int64>]"(i64 %input, i64 %output) {
entry:
  %input_ptr = inttoptr i64 %input to { i64*, i32*, i32* }*
  %output_ptr = inttoptr i64 %output to { i64*, i32*, i32* }*
  %shape_field = getelementptr { i64*, i32*, i32* }* %input_ptr, i64 0, i32 1
  %shape = load i32** %shape_field, align 8
  %dim = load i32* %shape, align 4
  br label %cond

after:                                            ; preds = %cond
  ret void

loop:                                             ; preds = %cond
  %data_field = getelementptr { i64*, i32*, i32* }* %output_ptr, i64 0, i32 0
  %data_addr = load i64** %data_field, align 8
  %stride_field = getelementptr { i64*, i32*, i32* }* %output_ptr, i64 0, i32 2
  %strides = load i32** %stride_field, align 8
  %stride0_ = load i32* %strides, align 4
  %offset_term = mul i32 %stride0_, %loop_idx.0
  %offsetCast = zext i32 %offset_term to i64
  %basePtrInt = ptrtoint i64* %data_addr to i64
  %idxAddrInt = add i64 %basePtrInt, %offsetCast
  %idxAddr = inttoptr i64 %idxAddrInt to <4 x i32>*
  %data_field3 = getelementptr { i64*, i32*, i32* }* %input_ptr, i64 0, i32 0
  %data_addr4 = load i64** %data_field3, align 8
  %stride_field5 = getelementptr { i64*, i32*, i32* }* %input_ptr, i64 0, i32 2
  %strides6 = load i32** %stride_field5, align 8
  %stride0_7 = load i32* %strides6, align 4
  %offset_term8 = mul i32 %stride0_7, %loop_idx.0
  %offsetCast9 = zext i32 %offset_term8 to i64
  %basePtrInt10 = ptrtoint i64* %data_addr4 to i64
  %idxAddrInt11 = add i64 %basePtrInt10, %offsetCast9
  %idxAddr12 = inttoptr i64 %idxAddrInt11 to <4 x i32>*
  %ret = load <4 x i32>* %idxAddr12, align 8
  %multmp = mul <4 x i32> %ret, <i32 3, i32 3, i32 3, i32 3>
  store <4 x i32> %multmp, <4 x i32>* %idxAddr, align 8
  %addtmp = add i32 %loop_idx.0, 4
  br label %cond

cond:                                             ; preds = %loop, %entry
  %loop_idx.0 = phi i32 [ 0, %entry ], [ %addtmp, %loop ]
  %cmptmp = icmp slt i32 %loop_idx.0, %dim
  br i1 %cmptmp, label %loop, label %after
}
