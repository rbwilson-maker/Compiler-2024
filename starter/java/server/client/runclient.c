/**
 * Based on nailgun client by <a href="http://www.martiansoftware.com/contact.html">Marty Lamb</a> and Pete Kirkham 
 * who is completely unimportant because Pete wrote the Win32 port, which is not used at all in this application.
 * 
 * Original file : ng.c
 * 
 * Yadda yadda apache licence here etc...
 * 
 * Edit 1 : check the validity of the args and print an appropriate message. 
 */

#include <assert.h>
#include <signal.h>
#include <stdio.h>

#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <byteswap.h>

#include <arpa/inet.h>
#include <netdb.h>
#include <netinet/in.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <sys/types.h>

// port we should connect to
#define SERVER_PORT         0xBABE
#define SERVER_ADDRESS      "127.0.0.1"

#define CLIENT_CONTINUE     0xCAFE00
#define CLIENT_NEWLOADER    0xCAFE01
#define CLIENT_NEWJOB       0xCAFE02
#define CLIENT_END          0xCAFE03

#define JOB_EXIT            0xCAFE04
#define JOB_EXCEPTION       0xCAFE05
#define JOB_OUT             0xCAFE06
#define JOB_ERR             0xCAFE07

#define EXCEPTION_SEGFAULT  0xCAFE08
#define EXCEPTION_ABORT     0xCAFE09
#define EXCEPTION_ALARM     0xCAFE0A
#define EXCEPTION_ARITH     0xCAFE0B
#define EXCEPTION_UNKNOWN   0xCAFE0C
#define EXCEPTION_INVALID   0xCAFE0D

#define SERVER_CONNECT_TIMEOUT 2

#define CHUNK_HEADER_LEN 5
#define BUFSIZE 2048

#define MATCH(a) (!strcmp(a, argv[pos]))

enum STATE {
  JOB,              // send job
  NEWLOADER,        // send new loader
  RESOURCES,        // send resources
  METHODNAME,       // send method name
  ARGS,             // send args
  NONE
};

bool keepListening = false;   // listen to the server after sending request?
bool emulate = true;          // emulate server termination?
bool connected = false;       // are we connected yet?


int socketConnection;
char buf[BUFSIZE];

inline void quit(int val){
  close(socketConnection);
  if(emulate) exit(val);
  else exit(0);
}

inline void exception(int val){
  close(socketConnection);
  if(emulate){
    /*switch(val){
      case EXCEPTION_SEGFAULT:
        raise(SIGSEGV);
      case EXCEPTION_ABORT:
        raise(SIGABRT);
      case EXCEPTION_ALARM:
        raise(SIGALRM);
      case EXCEPTION_ARITH:
        raise(SIGFPE);

      case EXCEPTION_UNKNOWN:
        printf("unknown error\n");
        raise(SIGILL);
      case EXCEPTION_INVALID:
        printf("invalid job request\n");
        raise(SIGILL);
    }*/
    exit(1);
  } else exit(0);
}

inline void exitServerDisconnect(){
  fprintf(stdout, 
    "**ERROR**"
    "\n\tThe server has closed our connection before we received an exit command"
    "\n\tThe server must be dead, or we have a malfomed request");
  close(socketConnection);
  exit(1);
}

unsigned long recvToBuffer(unsigned long len) {
  unsigned long bytesRead = 0;
  while(bytesRead < len) {
    //again, did not read the documentation. WAITALL causes this function to BLOCK
    //come on, guy
    int thisPass = recv(socketConnection, buf + bytesRead, len - bytesRead, MSG_WAITALL);
    if (thisPass == 0) {
      exitServerDisconnect();
    }
    bytesRead += thisPass;
  }
  return bytesRead;
}

uint32_t recvInt32(){
  recvToBuffer(4);
  uint32_t val = *((uint32_t*)buf);
  return __bswap_32(val);
}

uint32_t recvInt16(){
  recvToBuffer(2);
  uint16_t val = *((uint16_t*)buf);
  return __bswap_16(val);
}

void recvToFD(unsigned int destFD, char *buf, unsigned long len) {
  unsigned long bytesRead = 0;
  int bytesCopied;
  while (bytesRead < len) {
    unsigned long bytesRemaining = len - bytesRead;
    int bytesToRead = (BUFSIZE < bytesRemaining) ? BUFSIZE : bytesRemaining;
    int thisPass = 0;
    thisPass = recv(socketConnection, buf, bytesToRead, MSG_WAITALL);
    if (thisPass == 0) {
      exitServerDisconnect();
    }
    bytesRead += thisPass;
    bytesCopied = 0;
    while(bytesCopied < thisPass) {
      bytesCopied += write(destFD, buf + bytesCopied, thisPass - bytesCopied);
    }
  }
}

// ((OUTSTREAM|ERRSTREAM)[resourceSiz=K:16][serverOutput:K])*((EXIT|EXCEPTION)[exitType:32])
inline void processServerOutput(){
  switch(recvInt32()){
    case JOB_OUT: {
      uint16_t siz = recvInt16();
      recvToFD(STDOUT_FILENO, buf, siz);
      break;
    }
    case JOB_ERR: {
      uint16_t siz = recvInt16();
      recvToFD(STDERR_FILENO, buf, siz);
      break;
    }
    case JOB_EXIT:
      quit(recvInt32());
      break;
    case JOB_EXCEPTION:
      exception(recvInt32());
    default:
      assert(false);
  }
}

inline void connectToServer(){
  struct sockaddr_in server_addr;
  struct hostent* hostinfo = gethostbyname(SERVER_ADDRESS);
  socketConnection = socket(AF_INET, SOCK_STREAM, 0);

  server_addr.sin_family = AF_INET;
  server_addr.sin_port = htons(SERVER_PORT);
  server_addr.sin_addr = *(struct in_addr *) hostinfo->h_addr;
  memset(&(server_addr.sin_zero), '\0', 8);
  int val = connect(socketConnection, (struct sockaddr *)&server_addr, sizeof(struct sockaddr));

  while(val == -1) {
    usleep(1000 * 100);
    val = connect(socketConnection, (struct sockaddr *)&server_addr, sizeof(struct sockaddr));
  }
  connected = true;
}

/**
* Writes everything in the specified buffer to the specified
* socket handle.
*
* @param s the socket descriptor
* @param buf the buffer containing the data to send
* @param len the number of bytes to send. Also used to
* return the number of bytes sent.
* @return total bytes written or 0 if failure
*/
int sendAll(unsigned int s, char *buf, unsigned int len) {
  int total = 0;
  int bytesleft = len;
  int n = 0;
  while(total < len) {
    // the dude that wrote this did not read the documentation wtf
    // why is this in a while loop this is redundant
    n = send(s, buf+total, bytesleft, 0);
    if (n == -1) { break; }
    total += n;
    bytesleft -= n;
  }
  return n==-1 ? 0:total;
}

inline int sendInt32(uint32_t val){
  val = __bswap_32(val);
  return sendAll(socketConnection, (char*)(&val), 4);
}

inline int sendInt16(uint16_t val){
  val = __bswap_16(val);
  return sendAll(socketConnection, (char*)(&val), 2);
}

void sendString(char* string) {
  /* buffer used for reading and writing chunk headers */
  size_t len = strlen(string);  
  assert(len <= 0xFFFF);
  sendInt16((uint16_t)len);
  sendAll(socketConnection, string, len);
}

inline void sendRequest(int argc, char* argv[]){
  size_t pos = 1;
  STATE state = NONE;

  while(pos < argc){
    switch(state){
      case ARGS:
      case RESOURCES:
      case NONE: {
        STATE prev = state;
        if(MATCH("--exit-server")){
          sendInt32(CLIENT_END);
          state = NONE;
        } else if(MATCH("-ne")) {
          emulate = false;
        } else if(MATCH("-r")) {
          state = RESOURCES;
        } else if(MATCH("-m")) {
          state = METHODNAME;
        } else if(MATCH("-a")) {
          state = ARGS;
        } else if(MATCH("-n")) {
          state = NEWLOADER;
        } else if(MATCH("-j")) {
          keepListening = true;
          state = JOB;
        } else {
          assert(state == ARGS || state == RESOURCES);
          //continue
          sendInt32(CLIENT_CONTINUE);
          sendString(argv[pos]);
          break;
        }

        if(prev == ARGS || prev == RESOURCES) {
          sendInt32(CLIENT_END);
        }
      } break;
      case JOB:
        sendInt32(CLIENT_NEWJOB);
        sendInt32(atoi(argv[pos]));
        state = NONE;
        break;
      case NEWLOADER:
        sendInt32(CLIENT_NEWLOADER);
        sendInt32(atoi(argv[pos]));
        state = NONE;
        break;
      case METHODNAME:
        // send methodName argv[pos]
        sendString(argv[pos]);
        state = NONE;
        break;
    }
    pos++;
  }
  if(state == ARGS || state == RESOURCES) {
    sendInt32(CLIENT_END);
  }
}

void serverTimeoutHandler(int);
void printUsage();
void checkArgs(int argc, char* argv[]);

// Usage :  
// ** IMPORTANT ** you must provide ALL flags in the exact order
// Send a job : 
//   [-ne] -j loaderNum -r resources... -m methodname -a args...
// Make a new loader:
//   -n loaderNum -r resources...  (makes a new loader)
int runclient(int argc, char* argv[]){
  checkArgs(argc, argv);
  signal(SIGALRM, serverTimeoutHandler); // whoa I learned something in 213
  alarm(SERVER_CONNECT_TIMEOUT);

  //connect to the server
  connectToServer();

  //send information to the server
  sendRequest(argc, argv);

  int eof = 0;
  if(keepListening){
    while(true){
      processServerOutput();
    }
  }

  //clean up and exit
  quit(0);
  return 0;
}

void serverTimeoutHandler(int i){
  if(!connected){
    printf(
      "** ERROR **"
      "\n\tCould not connect to the server."
      "\n\tRun ./makeserver and try again.\n");
    exit(1);
  }
}

void printUsage(){
  printf(
    "== Usage ==\n"
    "Send a job : \n"
    "  [-ne] -j loaderNum -r resources... -m methodname -a args...\n"
    "Make a new loader : \n"
    "  -n loaderNum -r resources...\n"
    "Exit server : \n"
    "  --exit-server\n"
    "\n"

    "** IMPORTANT **\n"
    "  You must provide ALL flags in exact order\n");
}
void use(char* str){
  printf("Error : ");
  printf(str);
  printf("\n\n");

  printUsage();
  exit(0);
}

enum CheckArgType{
  ARG_NONE,   //unknown
  ARG_JOB,    //new job
  ARG_LOADER  //new loader
};

//returns true if string is a natural number
inline bool isNat(char* str){
  int v;
  int num = sscanf(str, "%d", &v); 
  return num == 1 && v >= 0;
}

inline size_t countChars(char* str, char v){
  size_t c = 0;
  for(size_t i = 0; i < strlen(str); i++){
    if(str[i] == v) c++;
  }
  return c;
}

void checkArgs(int argc, char* argv[]){
  if(argc == 1) use("no args specified");
  int pos = 1;
  CheckArgType type = ARG_NONE;

  if(MATCH("-ne")) {
    type = ARG_JOB;
    pos++;
    if(!MATCH("-j")){
      use("expected '-j'");
    }
  } else if(MATCH("--exit-server")) {
    if(argc > 2) use("Too many args specified for --exit-server");
    return;
  } else if(MATCH("-n")) {
    type = ARG_LOADER;
  } else if(MATCH("-j")) {
    type = ARG_JOB;
  } else{
    use("unrecognized flag");
  }
  
  if(argc < pos+3) use("not enough args specified");
  if(!isNat(argv[++pos])) use("expected decimal integer > 0 for loaderNum");

  pos++;
  if(!MATCH("-r")) use("expected -r");

  if(type==ARG_LOADER) return; 
    // we're done here on a LOADER. after -r can be anything

  while(pos < argc){
    if(MATCH("-m")) {
      goto method;
    }
    pos++;
  }
  use("expected -m");
method:
  
  if(argc < pos+2) use("not enough args specified");
  pos++;
  if(1 != countChars(argv[pos], '#')) 
    use("Invalid -m method specifier. Specify a method with <classPath>#<methodName>.");

  pos++;
  if(argc < pos+1 || !MATCH("-a")) use("expected -a");
  return; //we're done on a JOB. after -a can be anything
}