#--------------------------------------------
# Program to create SQL statements to populate
# the Slots table in the difxdb database
#
# Change settings below to reflect you
# library scheme
#
# Use the generated SQL to import into the database
# with the appropriate database client tool (e.g. mysql)
#
#-------------------------------------------- 
racks = ["CA", "CB", "CC", "DA", "DB", "DC"]
shelves = range(10)
slots = range(1,9)


file = open("createSlots.sql", "w")

file.write("USE difxdb\n")
file.write("INSERT INTO Slot (`location`) VALUES \n")
for rack in racks:
	for shelf in shelves:
		for slot in slots:
			location = "%s%s%02d" % (rack, shelf,slot)
			file.write("('%s'),\n" % location)

file.close()

