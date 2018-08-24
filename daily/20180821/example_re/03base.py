import timeit

timeit.repeat(
    stmt="rx.sub('',  '\r\n' * 20)",
    setup="import re; rx = re.compile('(\r\n|\r|\n)+\z')",
    repeat=2,
    number=1
)
# require 'benchmark'

# Benchmark.bm 2 do |r|
#   (20..24).each do |i|
#     r.report i do
#       ("\r\n"*i + "b").sub(/(\r\n|\r|\n)+\z/, "")
#     end
#   end
# end

