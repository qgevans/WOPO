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


#define CELL_DIGITS 3
#define BUFFER_SIZE 999
/*
 * 01 COBOL-BUFFER PIC 9(CELL_DIGITS) OCCURS BUFFER_SIZE TIMES
 */
char msg_buf[BUFFER_SIZE];

#define STATE_DIGITS 2
// 01 CHANNEL-STATUS PIC 9(STATE_DIGITS)
#define EBADDEST 10
#define EOPENFAIL 20
#define EHUP 30
#define ESERV 40

#define DEFAULT_PORT "6667"
int sockfd;

#define RECV_BUF_SIZE 1024
char recv_buf[RECV_BUF_SIZE];
size_t recv_buf_pos = 0;

void channel_set_status(char *state, int value)
{
  char buf[STATE_DIGITS + 1];
  snprintf(buf, STATE_DIGITS + 1, "%.*hhu", STATE_DIGITS, (unsigned char)value);
  memcpy(state, buf, STATE_DIGITS);

  return;
}

void channel_from_cobol(char *cobol_buffer)
{
  char buf[CELL_DIGITS + 1];
  buf[CELL_DIGITS] = '\0';
  memcpy(buf, cobol_buffer, CELL_DIGITS);

  int i;
  for(i = 0; strcmp(buf, "000") && i < BUFFER_SIZE; memcpy(buf, cobol_buffer + ++i * CELL_DIGITS, CELL_DIGITS)) {
    msg_buf[i] = (char)strtol(buf, NULL, 10);
  }
  msg_buf[i] = '\0';
  if (i == BUFFER_SIZE) {
    int message_length = 0;
    for(i = 0; i < BUFFER_SIZE; i++) {
      if(msg_buf[i] != ' ') {
	message_length = i + 1;
      }
    }
    if(message_length == BUFFER_SIZE) {
      message_length--;
    }
    msg_buf[message_length] = '\0';
  }
  return;
}

void channel_to_cobol(char *cobol_buffer)
{
  char buf[CELL_DIGITS + 1];
  for(int i = 0; i < BUFFER_SIZE && msg_buf[i]; i++) {
    snprintf(buf, CELL_DIGITS + 1, "%.*hhu", CELL_DIGITS, msg_buf[i]);
    memcpy(cobol_buffer + i * CELL_DIGITS, buf, CELL_DIGITS);
  }
  memset(cobol_buffer + (BUFFER_SIZE - 1) * CELL_DIGITS, (int)'0', CELL_DIGITS);

  return;
}

void channel_string_to_cobol(char *cobol_buffer, const char *s)
{
  strncpy(msg_buf, s, BUFFER_SIZE);
  msg_buf[BUFFER_SIZE - 1] = '\0';
  channel_to_cobol(cobol_buffer);
  return;
}

/*
 *         ASCII "HOST[:PORT]$NUL$" IN COBOL-BUFFER
 *         CALL "CHANNEL-OPEN" USING COBOL-BUFFER, STATE.
 */
void CHANNEL__OPEN(char *cobol_buffer, char *state)
{
  channel_from_cobol(cobol_buffer);
  if(!strlen(msg_buf)) {
    channel_string_to_cobol(cobol_buffer, "No host specified");
    channel_set_status(state, EBADDEST);
    return;
 }
  char *port = strchr(msg_buf, ':');
  if(port) {
    *port = '\0';
    port++;
    if(!strlen(port)) {
      channel_string_to_cobol(cobol_buffer, "Port separator specified, but not port");
      channel_set_status(state, EBADDEST);
      return;
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
  if((status = getaddrinfo(msg_buf, port, &hints, &res))) {
    channel_string_to_cobol(cobol_buffer, gai_strerror(status));
    channel_set_status(state, EBADDEST);
    return;
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
    channel_string_to_cobol(cobol_buffer, "Unable to connect to host");
    channel_set_status(state, EOPENFAIL);
    return;
  }

  channel_set_status(state, 0);
  return;
}

//         CALL "CHANNEL-SEND" USING COBOL-BUFFER, STATE.
void CHANNEL__SEND(char *cobol_buffer, char *state)
{
  char *msg;
  int sent, total;
  channel_from_cobol(cobol_buffer);
#ifdef DEBUG
  printf("Sending: %s\n", msg_buf);
#endif
  sent = 0;
  total = strlen(msg_buf);
  if(msg_buf[total - 2] != '\n') {
    if(total < BUFFER_SIZE) {
      total++;
    }
    msg_buf[total - 2] = '\n';
    msg_buf[total - 1] = '\0';
  }
  while(sent < total) {
    int status = send(sockfd, msg_buf + sent, total - sent, 0);
 if(status == -1) {
      perror("send");
      close(sockfd);
      channel_string_to_cobol(cobol_buffer, "Hung up");
      channel_set_status(state, EHUP);
      return;
    }
    sent += status;
  }

  channel_set_status(state, 0);
  return;
}

//         CALL "CHANNEL-RECV" USING COBOL-BUFFER, STATE.
void CHANNEL__RECV(char *cobol_buffer, char *state)
{
  char *message_end;
 get_buffered:
  message_end = memchr(recv_buf, '\n', recv_buf_pos);
  if(message_end) {
    size_t message_size = message_end - recv_buf;
    if (message_size > BUFFER_SIZE) {
      channel_string_to_cobol(cobol_buffer, "Server sent too-long message");
      channel_set_status(state, ESERV);
      return;
    }
    memcpy(msg_buf, recv_buf, message_size);
    msg_buf[message_size] = '\0';
    recv_buf_pos -= message_size + 1;
    message_end++;
    for(size_t i = 0; i < recv_buf_pos; i++) {
      recv_buf[i] = message_end[i];
    }
#ifdef DEBUG
    printf("Received: %s\n", msg_buf);
#endif
    channel_to_cobol(cobol_buffer);
    channel_set_status(state, 0);
    return;
  }
  if(recv_buf_pos < RECV_BUF_SIZE - 1) {
    ssize_t received = recv(sockfd, recv_buf + recv_buf_pos, RECV_BUF_SIZE - recv_buf_pos, 0);
    if(received != -1) {
      recv_buf_pos += received;
      goto get_buffered;
    }
    perror("recv");
    channel_string_to_cobol(cobol_buffer, "Hung up");
    channel_set_status(state, EHUP);
    return;
  }

  channel_string_to_cobol(cobol_buffer, "Server failed to send newline");
  channel_set_status(state, ESERV);
  return;
}

void CHANNEL__CLOSE(void)
{
  close(sockfd);

  return;
}

