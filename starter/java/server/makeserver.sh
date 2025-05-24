#/bin/bash
# sets up the testing server

# -----------------  client/client  ------------------
# Send a job : 
#   [-ne] -j loaderNum -r resources... -m methodname -a args...
# Make a new classloader : 
#   -n loaderNum -r resources...
# Close the server : 
#   --exit-server
#
# ** IMPORTANT **
#   ALL flags must be provided in the exact order
# ---------------------------------------------------


# 30 second server timeout between jobs
timeout=30000
SERVER_MAIN=edu.cmu.cs411.java.server.Main

if [ "$1" == "--SCALA" ] 
then
	scala -classpath server.jar $SERVER_MAIN -s -t $timeout &
else 
	java -jar server.jar -s -t $timeout &
fi 

# classloader 0 with no resources.
# you should send jobs to classloader 0 for testing
client/client -n 0 -r
