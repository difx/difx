# -*- coding: utf-8 -*-

import MySQLdb
from difxutil import parseKeyValue, splitObservationCode


class DiFXDBMySQLConnection:
	"""
	DB connection class that implements  MySQL
	"""

	def __init__(self, host="localhost", user="", password="", dbName="", conf="", verbosity=0):
		"""
		Constructor
		"""
		self.database = None 
		self.cursor = None
		self.isConnected = False

		self.conf = conf
		self.dbName = dbName
		self.host = host
		self.user = user
		self.password = password
		self.verbosity = verbosity

		self.connect()

		
	def connect(self):
		"""
		Opens the database connection
		"""
     
		try:
			if len(self.conf) > 0:
				self.database = MySQLdb.connect (read_default_file=self.conf, read_default_group="difxdb", sql_mode="TRADITIONAL")
			else:
				self.database = MySQLdb.connect (host = self.host ,user = self.user ,passwd = self.password ,db = self.dbName, sql_mode="TRADITIONAL")
			
			self.cursor = self.database.cursor(MySQLdb.cursors.DictCursor)
			

		except MySQLdb.Error, e:
			self.isConnected = False
			raise
	
		self.isConnected = True

	def close(self):
		"""
		Closes the database connection
		"""
		self.database.close()

	def executeSQL(self, sqlCommand, args=None):
		"""
		Executes an arbitrary SQL command
		"""
		self.__beVerbose(sqlCommand, args)

		self.cursor.execute(sqlCommand, args)
		

	def getData(self, sqlCommand, args=None):
		"""
		Returns a dictionary containing the result from the database query
		"""
		self.executeSQL(sqlCommand, args)
		return(self.cursor.fetchall())


	def __beVerbose(self, sqlCommand, args = None):
		"""
		Outputs the SQL statement if verbose > 0 has been set
		"""
		if self.verbosity > 0:

			if args != None:
				# make sure all arguments are converted to string
				cmd = sqlCommand % tuple(args)
			else:
				cmd = sqlCommand

			print "Executing SQL statement %s " % cmd



class DiFXDBTableItem(object):

	def __init__(self):
		self.primaryKey = None
		self.tableName = None
		self.fields = None
		self.excludeFromInsertFields = None

	def reset(self):
		for field in self.fields:
			setattr(self, field, None)

		setattr(self, self.primaryKey, None)

	def __repr__(self):

		str = ""
		for field in self.fields:	
			str += "%s = %s\n" % (field, getattr(self, field) )
		return str

	def __validateProperties(self):

		str = "Class %s does not implement the required tableName property" % (self.__class__.__name__)
		if not hasattr(self, 'tableName') or self.tableName == None or len(self.tableName) == 0:
			raise Exception (str)
		
	
	def fill (self,dbRecord):
		'''
		Populates the class propoerties from the given dbRecord object
		'''
		
		self.__validateProperties()
		self.reset()
	
		for field in self.fields:
			# first check if there is a field prefixed with the table name
			# that matches this field. This can occur if the same field name
			# is used in several table
			# if not use the unprefixed field
			prefixField = self.tableName + "." + field
			if prefixField in dbRecord.keys():
				setattr(self, field, dbRecord[prefixField])
			elif field in dbRecord.keys():
				setattr(self, field, dbRecord[field])

	def buildInsertStatement(self):

		""" 
		Builds a valid SQL INSERT statement based on the tableName, fields and excludeFromInsertFields properties.
		Derived subclasses must implement these properties in order for buildInsertStatement to function properly
		"""
		insertStatement = ""
		fieldCount = 0
		fieldStr = ""
		valueStr = ""
		valueList = []

		self.__validateProperties()

		for field in self.fields:
			# skip fields that should be excluded from INSERT statements
			if self.excludeFromInsertFields != None and field in self.excludeFromInsertFields:
				continue

			if getattr(self,field) == None:
				continue
			
			if fieldCount > 0:
				fieldStr += ","
				valueStr += ","
			fieldStr += field
			valueStr += "%s"
			
			valueList.append(getattr(self,field))
			
			fieldCount += 1

		if fieldCount > 0:
			insertStatement = "INSERT INTO %s (%s) VALUES (%s)" % (self.tableName, fieldStr, valueStr)
		
		return (insertStatement, valueList)

	def buildUpdateStatement(self):
		""" 
		Builds a valid SQL UPDATE statement based on the tableName, fields, excludeFromInsertFields and primaryKey properties.
		Derived subclasses must implement these properties in order for buildUpdateStatement to function properly
		"""
		statement = ""
		fieldCount = 0
		setStr = ""
		valueList = []

		self.__validateProperties()

		for field in self.fields:
			# skip fields that should be excluded from INSERT statements
			if self.excludeFromInsertFields != None and field in self.excludeFromInsertFields:
				continue

			if getattr(self,field) == None:
				continue
			
			if fieldCount > 0:
				setStr += ","
				
			setStr += field + "=" + "%s"
			
			valueList.append(getattr(self,field))
			
			fieldCount += 1

		if fieldCount > 0:
			statement = "UPDATE %s SET %s WHERE %s=%s" % (self.tableName, setStr, self.primaryKey, getattr(self,self.primaryKey))
		
		return (statement, valueList)
		
	def buildDeleteStatement(self):
		""" 
		Builds a valid SQL DELETE statement based on the tableName and primaryKey properties.
		Derived subclasses must implement these properties in order for buildDeleteStatement to function properly
		"""
		self.__validateProperties()

		statement = "DELETE FROM %s WHERE %s=%s" % (self.tableName, self.primaryKey, getattr(self,self.primaryKey))

		return(statement)

class DiFXDBPassItem(DiFXDBTableItem):

	def __init__(self):

		DiFXDBTableItem.__init__(self) 
		self.primaryKey  = "id"
		self.tableName = "Pass"
		self.fields = [self.primaryKey, "experimentID" , "passName", "passTypeID"]
		
		self.reset()

		
class DiFXDBExperimentItem(DiFXDBTableItem):

	def __init__(self):

		DiFXDBTableItem.__init__(self)
		self.primaryKey  = "id"
		self.tableName = "Experiment"
		self.fields = [self.primaryKey, "code" , "segment"]

		self.reset()

class DiFXDBJobItem(DiFXDBTableItem):

	def __init__(self):

		DiFXDBTableItem.__init__(self)
		self.primaryKey  = "id"
		self.tableName = "Job"
		self.fields = [self.primaryKey, "passID" , "jobNumber", "priority", "correlationStart", "correlationEnd","jobStart", 
				"jobDuration","queueTime","inputFile", "outputFile","outputSize","difxVersion","speedupFactor","numAntennas","numForeign",
				"dutyCycle","statusID", "status", "active"]

		self.excludeFromInsertFields = [self.primaryKey, "status", "active"]

		self.reset()

			
class DiFXDBQueueItem(object):

		
	def __init__(self, queueRecord = None):

		self.Experiment = DiFXDBExperimentItem()
		self.Pass = DiFXDBPassItem()
		self.Job = DiFXDBJobItem()

		if (queueRecord != None):
			self.Experiment.fill(queueRecord)
			self.Pass.fill( queueRecord)
			self.Job.fill( queueRecord)


class DiFXDBJobAction(object):

	@staticmethod
	def addJob(connection, job):
		"""
		Adds a new record into the Job table.
		Returns the id of the new record
		"""

		(sql, args) = job.buildInsertStatement()

		if len(args) > 0:
			connection.executeSQL(sql, args)

			job.id = connection.cursor.lastrowid
			return (job.id)
		else:
			return(None)

	@staticmethod
	def updateJob(connection, job):
		
		(sql, args) = job.buildUpdateStatement()


		if len(args) > 0:
			connection.executeSQL(sql, args)

			job.id = connection.cursor.lastrowid
			return (job.id)
		else:
			return(None)
	
	@staticmethod
	def deleteJob(connection, job):

		sql = job.buildDeleteStatement()

		connection.executeSQL(sql)
		
		

class DiFXDBPassTypeAction(object):
	'''
	Class contains the business logic concerning operations on the
	PassType database entity
	'''
	@staticmethod
	def getTypeIDByName(connection, type):
		"""
		Returns the id of the status record having the given type.
		Returns 'None'if no matching record exists in the JobType table
		"""

		sql = "SELECT id from PassType WHERE type = %s"
		args = [type]

		recordSet = connection.getData(sql, args)
		
		if (len(recordSet) > 0):
			return(recordSet[0]["id"])
		else:
			return(None)

class DiFXDBStatusAction(object):
	'''
	Class contains the business logic concerning operations on the
	JobStatus database entity
	'''
	@staticmethod
	def getStatusIDByName(connection, status):
		"""
		Returns the id of the status record having the given status.
		Returns 'None'if no matching record exists in the JobStatus table
		"""

		sql = "SELECT id from JobStatus WHERE status = %s"
		args = [status]

		recordSet = connection.getData(sql, args)
		
		if (len(recordSet) > 0):
			return(recordSet[0]["id"])
		else:
			return(None)
	
class DiFXDBPassAction(object):
	'''
	Class contains the business logic concerning operations on the
	Pass database entity
	'''
	@staticmethod
	def addPass(connection, Pass):
		"""
		Adds a new record into the Pass table.
		Returns the id of the new record
		Input:
			connection: the database connection object
			Pass (DiFXDBPassItem)
		"""

		(sql, args) = Pass.buildInsertStatement()

		if len(args) > 0:
			connection.executeSQL(sql, args)

			Pass.id = connection.cursor.lastrowid
			return (Pass.id)
		else:
			return(None)


	
	@staticmethod
	def getPassID(connection, passName, experimentID):
		"""
		Returns the primary key of the pass record with the given passName and experimentID
		Returns 'None'if no matching record exists in the Pass table
		Input:
			connection: the database connection object
			passName (String)
			experimentID (Integer)
		"""

		sql = "SELECT id FROM Pass WHERE experimentID = %s AND passName = %s "
		args = [experimentID, passName]

		recordSet = connection.getData(sql, args)
		
		if (len(recordSet) > 0):
			return(recordSet[0]["id"])
		else:
			return(None)

	@staticmethod
	def updatePass(connection, Pass):
		
		(sql, args) = Pass.buildUpdateStatement()


		if len(args) > 0:
			connection.executeSQL(sql, args)

			Pass.id = connection.cursor.lastrowid
			return (Pass.id)
		else:
			return(None)

	@staticmethod
	def deletePass(connection, Pass):
		"""
		Deletes the pass record referenced by the given Pass object
		Input:
			connection: the database connection object
			Pass (DiFXDBPassItem)
		"""

		sql = Pass.buildDeleteStatement()

		connection.executeSQL(sql)


	@staticmethod
	def getJobCount(connection, Pass):
		"""
		Returns the number of jobs asscociated with the given pass.
		Input:
			connection: the database connection object
			Pass (DiFXDBPassItem): the pass to check for jobs
		"""
		
		sql = " SELECT id FROM Job WHERE passID = %s "
		args = [Pass.id]

		recordSet = connection.getData(sql, args)
	
		return(len(recordSet))

	@staticmethod
	def getType(connection, id):
		"""
		Returns the type name of the pass ascociated with the given id
		Returns 'None'if no matching record exists in the Pass table
		Input:
			connection: the database connection object
			id (integer): the pass id 
		"""
		
		sql = " SELECT type FROM Pass INNER JOIN PassType ON (Pass.passTypeID = PassType.id) WHERE Pass.id = %s "
		args = [id]

		recordSet = connection.getData(sql, args)

		if (len(recordSet) > 0):
			return(recordSet[0]["type"])
		else:
			return(None)



class DiFXDBExperimentAction(object):
	'''
	Class contains the business logic concerning operations on the
	Experiment database entity
	'''
	@staticmethod
	def addPass(connection, Pass):
		"""
		Adds a new record into the Pass table.
		Returns the id of the new record
		Input:
			connection: the database connection object
			Pass (DiFXDBPassItem)
		"""

		(sql, args) = Pass.buildInsertStatement()

		if len(args) > 0:
			connection.executeSQL(sql, args)

			Pass.id = connection.cursor.lastrowid
			return (Pass.id)
		else:
			return(None)

	@staticmethod
	def addExperiment(connection, experiment):
		"""
		Adds a new record into the Experiment table.
		Returns the id of the new record
		"""

		(sql, args) = experiment.buildInsertStatement()

		if len(args) > 0:
			connection.executeSQL(sql, args)

			experiment.id = connection.cursor.lastrowid
			return (experiment.id)
		else:
			return(None)
		

	@staticmethod
	def getExperimentID(connection, code, segment=None):
		"""
		Returns the primary key of the experiment record with the given code and (optional) segment value.
		Returns 'None'if no matching record exists in the Experiment table
		"""

		if (segment != None):
			sql = "SELECT id FROM Experiment WHERE code = %s AND segment = %s"
			args = [code, segment]
		else:
			sql = "SELECT id FROM Experiment WHERE code = %s AND segment IS NULL"
			args = [code]

		recordSet = connection.getData(sql, args)
		
		if (len(recordSet) > 0):
			return(recordSet[0]["id"])
		else:
			return(None)

	@staticmethod
	def getPassCount(connection, experiment):
		"""
		Returns the number of passes asscociated with the given experiment.
		Input:
			connection: the database connection object
			experiment (DiFXDBExperimentItem): the experiment to check for passes
		"""
		
		sql = " SELECT id FROM Pass WHERE experimentID = %s "
		args = [experiment.id]

		recordSet = connection.getData(sql, args)
	
		return(len(recordSet))

	
	@staticmethod
	def deleteExperiment(connection, experiment):
		"""
		Deletes the experiment record referenced by the given Experiment object
		Input:
			connection: the database connection object
			experiment (DiFXDBExperimentItem)
		"""

		sql = experiment.buildDeleteStatement()

		connection.executeSQL(sql)



class DiFXDBQueueAction(object):


	@staticmethod 
	def getQueue(connection, condition = "", order= ""):

		D = []
		
		query = "select * from Job JOIN Pass ON Job.passID = Pass.id JOIN Experiment ON Pass.experimentID = Experiment.id JOIN JobStatus ON JobStatus.id = Job.statusID JOIN PassType ON PassType.id = Pass.passTypeID "

		if len(condition) > 0:
			query +=  condition

		if (len(order)) > 0:
			query += order

	
		connection.executeSQL(query)


		for row in connection.cursor.fetchall():
			D.append(DiFXDBQueueItem(row))

		return D

	@staticmethod
	def getPasses (connection, experiment="", passes=[]):

		passCount = 0
		result = []
		condition = ""

		if experiment == None or len(experiment) == 0:
			raise Exception ("DiFXDBQueueAction::getPasses: empty experiment parameter.")

		(code, segment) = splitObservationCode(experiment)
		if len(segment) > 0:
			condition = " WHERE (code='%s' AND segment='%s') " % (code, segment)
		else:
			condition = " WHERE (code='%s') " % code

		if passes != None and len(passes) > 0:
			for passName in passes:

				conStr = " ( passName='%s' ) " % passName
				if (passCount == 0):
					condition += " AND ( "  + conStr
				else:
					condition += " OR " + conStr
				
				passCount += 1
			condition += " ) "
		condition += " GROUP BY Pass.id "
		
		print condition
		result = DiFXDBQueueAction.getQueue(connection, condition)

		return result
		
	@staticmethod 
	def getJobs(connection, experiment= "", passes= None, jobList=None, isActive= False, order = ""):

		# TODO: implement order functionality

		result=[]
		condition = ""
		code = ""
		segment = ""
		passCount = 0
		jobCount = 0

		if experiment == None or len(experiment) == 0:
			raise Exception ("DiFXDBQueueAction::getJobs: empty experiment parameter.")
		
		(code, segment) = splitObservationCode(experiment)
		if len(segment) > 0:
			condition = " WHERE (code='%s' AND segment='%s') " % (code, segment)
		else:
			condition = " WHERE (code='%s') " % code

		
		if passes != None and len(passes) > 0:
			for passName in passes:

				conStr = " ( passName='%s' ) " % passName
				if (passCount == 0):
					condition += " AND ( "  + conStr
				else:
					condition += " OR " + conStr
				
				passCount += 1
			condition += " ) "

		if jobList != None and len(jobList) > 0:
			for job in jobList:
				conStr = " ( jobNumber='%s' ) " % job
				if (jobCount == 0):
					condition += " AND ( "  + conStr
				else:
					condition += " OR " + conStr
				
				jobCount += 1
			condition += " ) "
				

		if (isActive == True):
			condition += " AND active = 1 "

		result = DiFXDBQueueAction.getQueue(connection, condition, order)

		return result
		

	
	@staticmethod 
	def getIncompleteJobs(connection, experiments=None, order=""):
		
		# TODO: implement order functionality
		result=[]
		condition = ""
		expCount = 0
		passCount = 0

		if experiments != None:
			for experiment in experiments:
				code = ""
				segment = ""
				(code, segment) = splitObservationCode(experiment)

				if len(segment) > 0:
					conStr = " (code='%s' AND segment='%s') " % (code, segment)
				else:
					conStr = " code='%s' " % code

				if (expCount == 0):
					condition = " WHERE "  + conStr
				else:
					condition += " OR " + conStr
				
				expCount += 1

				

		if len(condition) > 0:
			condition += " AND active = 1 "
		else:
			condition = " WHERE active = 1 "

		result = DiFXDBQueueAction.getQueue(connection, condition, order)

		return result
	
	@staticmethod 
	def getIncompleteJobsByPasses(connection, experiment, passes=None, order=""):
		
		result = DiFXDBQueueAction.getJobs(connection, experiment, passes, None, True, order)

		return result


	@staticmethod 
	def getIncompleteJobsByPasses_Deprecated (connection, experiment, passes=None, order=""):

		result=[]
		condition = ""
		code = ""
		segment = ""
		passCount = 0

		if experiment == None or len(experiment) == 0:
			raise Exception ("DiFXDBQueueAction::getIncompleteJobsByPasses: empty experiment parameter.")
		#if passes == None or len(passes) == 0:
		#	raise Exception ("DiFXDBQueueAction::getIncompleteJobsByPasses: empty passes parameter.")

		(code, segment) = splitObservationCode(experiment)
		if len(segment) > 0:
			condition = " WHERE (code='%s' AND segment='%s') " % (code, segment)
		else:
			condition = " WHERE (code='%s') " % code

		
		if passes != None and len(passes) > 0:
			for passName in passes:

				conStr = " ( passName='%s' ) " % passName
				if (passCount == 0):
					condition += " AND ( "  + conStr
				else:
					condition += " OR " + conStr
				
				passCount += 1
			condition += " ) "

		condition += " AND active = 1 "

		result = DiFXDBQueueAction.getQueue(connection, condition, order)

		return result
		
	@staticmethod 
	def getActiveJobs(connection, order = ""):

		condition = " WHERE active = 1 "
		result = DiFXDBQueueAction.getQueue(connection, condition, order)

		return result


	


	@staticmethod 
	def addQueueItem(connection, queueItem):
	
		
		# verify that the requested PassType exists in the database
		typeID = None
		if queueItem.Pass.passType != None:
			typeID = DiFXDBPassTypeAction.getTypeIDByName(connection, queueItem.Pass.passType)
			if typeID == None:
				raise Exception("The requested pass type ( %s )does not exist in the database" % queueItem.Pass.passType)

		

		# add to Experiment table
		expID = DiFXDBExperimentAction.getExperimentID(connection,queueItem.Experiment.code, queueItem.Experiment.segment)
		if (expID == None):
			expID = DiFXDBExperimentAction.addExperiment(connection,queueItem.Experiment)
		
		queueItem.Pass.experimentID = expID
		
		# add to Pass tableName
		if typeID == None:
			queueItem.Pass.passTypeID = DiFXDBTypeAction.getTypeIDByName(connection, "production")
		else:	
			queueItem.Pass.passTypeID = typeID

		

		passID = DiFXDBPassAction.getPassID(connection, queueItem.Pass.passName, queueItem.Pass.experimentID)
		if (passID == None):
			passID = DiFXDBPassAction.addPass(connection,queueItem.Pass)
		else:
			# get passType of existing pass
			typeName = DiFXDBPassAction.getType(connection, passID)
			if (typeName != queueItem.Pass.passType):
				raise Exception("Cannot change pass type to '%s'. Pass already has its type set to '%s'" % (queueItem.Pass.passType, typeName))
		

		queueItem.Job.passID = passID
		
		queueItem.Job.statusID = DiFXDBStatusAction.getStatusIDByName(connection, "queued")
		

		# Add to the Job table
		DiFXDBJobAction.addJob(connection, queueItem.Job)
	

		return
	
	@staticmethod 
	def deleteQueueItem(connection, queueItem):
	
		jobCount = -1
		expCount = -1
		
		passID = queueItem.Job.passID
		expID = queueItem.Pass.experimentID

		# delete Job 
		DiFXDBJobAction.deleteJob(connection, queueItem.Job)

		# also delete the pass if it has no more jobs
		jobCount = DiFXDBPassAction.getJobCount(connection, queueItem.Pass)
		
		if (jobCount == 0):
			DiFXDBPassAction.deletePass(connection, queueItem.Pass)
		
		# finally also delete the experiment if it has no more passes
		expCount = DiFXDBExperimentAction.getPassCount(connection, queueItem.Experiment)

		if (expCount == 0):
			DiFXDBExperimentAction.deleteExperiment(connection, queueItem.Experiment)