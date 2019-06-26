module Kernel
  def puts(x)
    raise RuntimeError.new("hmm")
  end
end

puts("hello")
