
import re
from datetime import datetime


for x in xrange(1,27):
    regex = 'a?' * x + 'a' * x
    string = 'a' * x
    r = re.compile(regex)
    start = datetime.now()
    r.match(string)
    print x, datetime.now() - start

