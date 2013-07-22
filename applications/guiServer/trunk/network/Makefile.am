INCLUDES = -I$(top_srcdir)

h_files = \
	GenericSocket.h \
	TCPSocket.h \
	TCPServer.h \
	TCPClient.h \
	UDPSocket.h

noinst_PROGRAMS = \
	clientTest \
	serverTest \
	udpTest

clientTest_SOURCES = \
	$(h_files) \
	clientTest.cpp

serverTest_SOURCES = \
	$(h_files) \
	serverTest.cpp

udpTest_SOURCES = \
	$(h_files) \
	udpTest.cpp
