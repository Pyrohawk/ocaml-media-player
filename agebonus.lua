-- If you have lua installed, you can run this as "lua agebonus.lua".

step = math.tan (1) / 99
for i = 1, 100, 1 do
  -- The 3 exponential exagerates the curve, IOW, flattening it more
  -- at the extremes
  n = 100 * (math.atan (step * (100 - i)) ^ 3)
  io.write (string.format ("%.2f\n", n))
end

