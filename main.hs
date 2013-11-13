import Language.Haskell.Interpreter (eval, runInterpreter, Interpreter, InterpreterError)

a = 71.04
c = 103.01
d = 115.03
e = 129.04
f = 147.07
g = 57.02
h = 137.06
i = 113.08
m = 131.04
n = 114.04
p = 97.05
q = 128.06
r = 156.1
s = 87.03
t = 101.05
v = 99.07
w = 186.08
y = 163.06
_3a = 85.06
_3c = 117.03
_3d = 192.05
_3e = 143.06
_3f = 161.09
_3g = 71.04
_3h = 151.08
_3i = 127.1
_3k = 142.11
_3l = 127.1
_3n = 128.06
_3p = 111.07
_3q = 142.07
_3r = 170.12
_3s = 101.05
_3t = 115.07
_3v = 113.09
_3w = 200.1
_3y = 117.08
x = 111.07
z = 112.06
u = 85.05

main = do
  print "What character do you want?"
  var <- getChar
  res <- parse var
  print res
