#include <sys/types.h>
#include <sys/socket.h>
#include <ctype.h>
#include <errno.h>
#include <limits.h>
#include <netdb.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#define LENGTH_DIGITS 3
#define DEFAULT_PORT "6667"
#define RECV_BUF_SIZE 1024
/*
 * 01 BUFFER
 *     03 MSG-LENGTH PIC 9(LENGTH_DIGITS)
 *     03 MSG-BODY PIC X(x)
 */
#define BODY(buf) (char *)(buf + LENGTH_DIGITS)

// 01 CHANNEL-STATUS PIC 9(2)
#define EBADDEST 10
#define EOPENFAIL 20
#define EHUP 30
#define ESERV 40

char *msg_length, *msg_body;
int msg_body_len;
int sockfd;
char recv_buf[RECV_BUF_SIZE];
size_t recv_buf_pos = 0;

int channel_message_length(void)
{
  char c_length[LENGTH_DIGITS + 1];
  memcpy(c_length, msg_length, LENGTH_DIGITS);
  c_length[LENGTH_DIGITS] = '\0';
  return (int)strtol(c_length, NULL, 10) - 1;
}

void channel_from_cobol(void)
{
  int message_length = channel_message_length();
  memset(msg_body + message_length, 0, msg_body_len - message_length);
  for(char *c = msg_body; *c; c++) {
    *c = tolower(*c);
  }
  return;
}

void channel_to_cobol(void)
{
  char *c;
  for(c = msg_body; *c; c++) {
    *c = toupper(*c);
  }
  memset(c, ' ', msg_body_len - strlen(msg_body));

  return;
}

void channel_string_to_cobol(const char *s)
{
  strncpy(msg_body, s, msg_body_len - 2);
  msg_body[msg_body_len - 1] = '\0';
  channel_to_cobol();
  return;
}

/*
 *         MOVE LENGTH OF MSG-BODY OF BUFFER TO MSG-LENGTH OF BUFFER.
 *         CALL "CHANNEL-INIT" USING BUFFER.
 */
void CHANNEL__INIT(char *buffer)
{
  msg_length = buffer;
  msg_body = BODY(buffer);
  msg_body_len = channel_message_length();

  return;
}

/*
 *         STRING "HOST[:PORT]"
 *                INTO MSG-BODY
 *                WITH POINTER MSG-LENGTH.
 *         CALL "CHANNEL-OPEN" GIVING CHANNEL-STATUS.
 */
int CHANNEL__OPEN(void)
{
  channel_from_cobol();
  if(!strlen(msg_body)) {
    channel_string_to_cobol("No host specified");
    return EBADDEST;
 }
  char *port = strchr(msg_body, ':');
  if(port) {
    *port = '\0';
    port++;
    if(!strlen(port)) {
      channel_string_to_cobol("Port separator specified, but not port");
      return EBADDEST;
    }
  } else {
    port = DEFAULT_PORT;
  }

  struct addrinfo hints, *res;
  int status;
  memset(&hints, 0, sizeof(hints));
  hints.ai_family = AF_UNSPEC;
  hints.ai_socktype = SOCK_STREAM;
  hints.ai_flags = AI_ADDRCONFIG;
  if((status = getaddrinfo(msg_body, port, &hints, &res))) {
    channel_string_to_cobol(gai_strerror(status));
    return EBADDEST;
  }

  struct addrinfo *curr_addr;
  for(curr_addr = res; curr_addr; curr_addr = curr_addr->ai_next) {
    sockfd = socket(curr_addr->ai_family, curr_addr->ai_socktype, curr_addr->ai_protocol);
    if(sockfd == -1) {
      perror("socket");
      continue;
    }
    if(connect(sockfd, curr_addr->ai_addr, curr_addr->ai_addrlen) == -1) {
 perror("connect");
      close(sockfd);
      continue;
    }

    break;
  }
  
  if(!curr_addr) {
    channel_string_to_cobol("Unable to connect to host");
    return EOPENFAIL;
  }

  return 0;
}

int CHANNEL__SEND(void)
{
  char *msg;
  int sent, total;
  channel_from_cobol();
  sent = 0;
  total = strlen(msg_body);
  msg_body[total++] = '\n';
  while(sent < total) {
    int status = send(sockfd, msg_body + sent, total - sent, 0);
    if(status == -1) {
      perror("send");
      close(sockfd);
      channel_string_to_cobol("Hung up");
      return EHUP;
    }
    sent += status;
  }

  return 0;
}

int CHANNEL__RECV(void)
{
  char *message_end;
 get_buffered:
  message_end = memchr(recv_buf, '\n', recv_buf_pos);
  if(message_end) {
    size_t message_size = message_end - recv_buf;
    if (message_size > msg_body_len) {
      channel_string_to_cobol("Server sent too-long message");
      return ESERV;
    }
    memcpy(msg_body, recv_buf, message_size);
    msg_body[message_size - 1] = '\0';
    recv_buf_pos -= message_size + 1;
    message_end++;
    for(size_t i = 0; i < recv_buf_pos; i++) {
      recv_buf[i] = message_end[i];
    }
    channel_to_cobol();
    return 0;
  }
  if(recv_buf_pos < RECV_BUF_SIZE - 1) {
    ssize_t received = recv(sockfd, recv_buf + recv_buf_pos, RECV_BUF_SIZE - recv_buf_pos, 0);
    if(received != -1) {
      recv_buf_pos += received;
      goto get_buffered;
    }
    perror("recv");
    channel_string_to_cobol("Hung up");
    return EHUP;
  }

  channel_string_to_cobol("Server failed to send newline");
  return ESERV;
}

void CHANNEL__CLOSE(void)
{
  close(sockfd);

  return;
}

