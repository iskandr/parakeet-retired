
type source_info = {
  filename: string option;
  line: int; col: int; 
}

let emptySourceInfo = { 
  line = 0; col = 0; 
  filename=None 
}

let mk_source_info ?filename line col = {line=line; col= col; filename=filename}