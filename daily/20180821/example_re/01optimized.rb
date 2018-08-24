require 'benchmark'

Benchmark.bm 2 do |r|
  (20..24).each do |i|
    r.report i do
      ("\r\n"*i + "b").sub(/(?>\r\n|\r|\n)+\z/, "")
    end
  end
end
