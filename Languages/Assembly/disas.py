#!/usr/bin/python

import sys
import re
import os

TMP_ASM_FILE = "/tmp/__X.S"
TMP_OBJ_FILE = "/tmp/__X.o"

def compile_64_bit(f, data_bytes):
  f.write(".byte %s\n" %  ", ".join(data_bytes))
  f.flush()
  os.system("as %s -o %s" % (TMP_ASM_FILE, TMP_OBJ_FILE))

def compile_32_bit(f, data_bytes):
  f.write("SECTION .text\n")
  f.write("BITS 16\n")
  f.write("db %s\n" %  ", ".join(data_bytes))
  f.flush()
  os.system("nasm -f elf32 %s -o %s" % (TMP_ASM_FILE, TMP_OBJ_FILE))

def main():
  mode = "64"
  if len(sys.argv) > 1:
    mode = sys.argv[1]
  data = sys.stdin.read()
  data_bytes = re.findall("0x[A-fa-f0-9]{1,2}", data)
  with file("/tmp/__X.S", "w") as f:
    if mode == "64":
      compile_64_bit(f, data_bytes)
    elif mode == "32":
      compile_32_bit(f, data_bytes)
  disas_data = os.popen("objdump -hd %s" % TMP_OBJ_FILE).read()
  os.unlink(TMP_OBJ_FILE)
  os.unlink(TMP_ASM_FILE)

  print_flag = False
  for line in disas_data.splitlines():
    if "<.text>:" in line:
      print_flag = True
      continue

    if print_flag:
      print line
    

  
if __name__ == '__main__':
  main()
