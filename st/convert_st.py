import os

def ensure_dir(path):	
	if not os.path.exists(path):
		os.makedirs(path)

def convert_dir(dir_path):
	dirname = os.path.dirname(dir_path)
	base = os.path.basename(dir_path)
	saved_dir = dirname + "/" + base + "_converted/"
	ensure_dir(saved_dir)
	for root, dirs, files in os.walk(dir_path):
		for name in files:
			filename = os.path.join(root, name)
			convert_file(filename, saved_dir)
		for name in dirs:
			dirname = os.path.join(root, name)
			convert_dir(dirname)
			pass
		pass
	pass

def convert_file(filename, dirname):
	
	result = []
	base = os.path.basename(filename)
	parts = base.split(".")

	classname = parts[0]
	result.append("%s extend [\n" % classname)
	new_name = os.path.join(dirname, base)
	with open(filename) as f:
		content = f.readlines()
		pass

	for line in content:
		if line == "!\n":
			result.append("    ]\n")
			continue
		if line == "! !\n":
			#print "two exiles detected"
			result.append("    ]\n")
			continue
			#print line
		if line.find("!") != -1:
			continue
			#print "category decteced"
			#print line
		
		strtest = line[0]
		if not strtest.isspace():
			line = line.replace("\n"," [\n")
		result.append("    "+line)
		pass

	result.append("]")
	new_content = "".join(result)
	new_file = open(new_name, "w")
	new_file.write(new_content)
	new_file.close()
	pass

convert_dir("/home/gloryofrobots/develop/smalltalk/yellowtalk/st/core")
