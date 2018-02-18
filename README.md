# Java-Interpreter
A simple Java interpreter implemented with Scheme

# Testing Your Interpreter, Part 1

### Test 1: This code should return 150.

return 150;

### Test 2: This code should return -4.

return 6 * (8 + (5 % 3)) / 11 - 9;

### Test 3: This code should return 10.

var z;
z = 10;
return z;

### Test 4: This code should return 16.

var x = (5 * 7 - 3) / 2;
return x;

### Test 5: This code should return 220.

var x = 10;
var y = 12 + x;
return x * y;

### Test 6: This code should return 5.

var x = 5;
var y = 6;
var m;
if (x <= y)
  m = x;
else
  m = y;
return m;

### Test 7: This code should return 6.

var x = 5;
var y = 6;
var m;
if (x >= y)
  m = x;
else
  m = y;
return m;

### Test 8: This code should return 10.

var x = 5;
var y = 6;
if (x != y)
  x = 10;
return x;

### Test 9: This code should return 5.

var x = 5;
var y = 6;
if (x == y)
  x = 10;
return x;

### Test 10: This code should return -39.

return 6 * -(4 * 2) + 9;

### Test 11: This code should give an error (using before declaring).

var x = 1;
y = 10 + x;
return y;

### Test 12: This code should give an error (using before declaring).

var y;
y = x;
return y;

### Test 13: This code should give an error (using before assigning).

var x;
var y;
x = x + y;
return x;

### Test 14: This code should give an error (redefining). This is not a required error, but it would be nice if you could catch these.

var x = 10;
var y = 20;
var x = x + y;
return x;

### Test 15: This code should return true (not #t).

return (10 > 20) || (5 - 6 < 10) && true;

### Test 16: This code should return 100.

var x = 10;
var y = 20;
if (x < y && (x % 2) == 0)
  return 100;
else
  return 200;

### Test 17: This code should return false (not #f).

var x = 100 % 2 == 0;
var y = 10 >= 20;
var z;
if (x || y)
  z = y;
else
  z = x;
return z;

### Test 18: This code should return true.

var x = 10;
var y = 20;
var z = 20 >= 10;
if (!z || false)
  z = !z;
else
  z = z;
return z;

### Test 19: This code should return 128.

var x = 2;
while (x < 100)
  x = x * 2;
return x;

### Test 20: This code should return 12;

var x = 20;
var y = 128;
while (x * x > 128)
  x = x - 1;
x = x + 1;
return x;

Additional Tests for Students Looking for an Extra Challenge

### Test 21: This code should return 30.

var x;
var y;
var z = x = y = 10;
return x + y + z;

### Test 22: This code should return 11.

var x;
var y;
x = y = 10;
if ((x = x + 1) > y)
  return x;
else
  return y;

### Test 23: This code should return 1106.

var x;
var y = (x = 5) + (x = 6);
return y * 100 + x;

### Test 24: This code should return 12.

var x = 10;
x = (x = 6) + x;
return x;

### Test 25: This code should return 16.

var x = 10;
x = x + (x = 6);
return x;

### Test 26: This code should return 72.

var x;
var y;
var z;
var w = (x = 6) + (y = z = 20);
return w + x + y + z;

### Test 27: This code should return 21.

var x = 0;
while ((x = x + 1) < 21)
  x = x;
return x;

### Test 28: This code should return 164.

var a = 31160;
var b = 1476;
var r = a % b;
while (r != 0)
  r = (a = b) % (b = r);
return b;
