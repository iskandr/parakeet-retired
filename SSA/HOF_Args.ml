
type ('a, 'b) closure = {
  
}

type 'a hof_args = { 
  axes : int list; 
  init : ('a list) option; 
  args : 'a list  
} 