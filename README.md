qrcode
======

QR Code Generator in Racket

- Encoding only 
- QR model 2 codes, versions (1-40)
- Only byte mode supported presently


Command line usage:
```
racket qrcode.rkt -l Q hello.png Hello QR World
```

References:

- ISO/IEC STANDARD 18004 (2006)
- Error Control Coding (2nd Edition) [control code theory]
- http://en.wikiversity.org/wiki/Reed–Solomon_codes_for_coders [helpful for implementation tips]
