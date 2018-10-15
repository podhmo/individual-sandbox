for i in range(10):
    i  # => 9
    j = i * i
    j  # => 81
    for k in range(2): # =>
        (k, j, i) # => (1, 81, 9)
