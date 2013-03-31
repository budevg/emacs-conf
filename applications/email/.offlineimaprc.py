import subprocess
def my_get_output(msg):
  # Bunch of boilerplate to catch the output of a command:
  pipe = subprocess.Popen("/usr/bin/ssh-askpass %s" % msg, shell=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
  (output, errout) = pipe.communicate()
  assert pipe.returncode == 0 and not errout
  return output.strip().lstrip('"').rstrip('"')
