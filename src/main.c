/* Adapted from https://github.com/mafintosh/echo-servers.c
 * License text below: */

/*****************************************************
 The MIT License (MIT)

Copyright (c) 2014 Mathias Buus

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*****************************************************/

#include "logging.h"
#include "run_command.h"

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <unistd.h>
#include <errno.h>
#include <libssh/libssh.h>

#define BUFFER_INCREMENT_SIZE 8192
//minimum read size--if we don't have at least this much space then increase the buffer size
#define BUFFER_MIN_FREE 2048

#define HTTP_END "\r\n\r\n"

/**
 * ***WARNING*** BUFFER RETURNED IN ret_buf IS -CALLER-FREE-
 */
void read_socket(int client_fd, char** ret_buf, size_t* ret_buf_size)
{
    //set parameters to error values in case something goes wrong
    *ret_buf = NULL;

    ssize_t buffer_size = BUFFER_INCREMENT_SIZE,
            buffer_available_space = BUFFER_INCREMENT_SIZE,
            buffer_pos = 0;
    char* recv_buffer = malloc(buffer_size);

    while(true)
    {
        int read = recv(client_fd, recv_buffer, buffer_available_space, buffer_pos);

        if(read < 0 && errno == EINTR)
        {
            continue;
        }
        else if(read < 0)
        {
            die("Client read failed\n");
        }
        else if(read == 0)
        {
            break;
        }
        else
        {
            //still reading
            
            //see if we need to allocate more space
            buffer_available_space -= read;
            buffer_pos += read;

            //check if we need to allocate more memory for the buffer
            if(buffer_available_space < BUFFER_MIN_FREE)
            {
                //reallocate it the new buffer size
                buffer_size += BUFFER_INCREMENT_SIZE;
                recv_buffer = realloc(recv_buffer, buffer_size);
                buffer_available_space += BUFFER_INCREMENT_SIZE;
                //the read position stays the same
                log_debug("Expanded read buffer.  Old size (bytes): %d, new size: %d\n", 
                        buffer_size - BUFFER_INCREMENT_SIZE, buffer_size);
            }

            //add a null terminator both for logging and just in case
            //it'll just get overwritten if we have more left to read
            recv_buffer[buffer_pos + 1] = '\0';
            log_debug("Read block from client: %s\n", recv_buffer);

            //check if we're done reading
            //\r\n\r\n signals end of HTTP request
            
            if(strstr(recv_buffer, HTTP_END) != NULL)
            {
                log_debug("HTTP end of request detected");
                break;
            }
        }
    }

    //write out return values
    *ret_buf = recv_buffer;
    *ret_buf_size = buffer_size - buffer_available_space; //return the amount of actual used space
}

int main (int argc, char *argv[])
{
    if (argc < 2)
    {
        fprintf(stderr, "Usage: %s [port]\n", argv[0]);
        exit(1);
    }

    uint16_t port = atol(argv[1]);

    int server_fd, client_fd, err;
    struct sockaddr_in server, client;

    server_fd = socket(AF_INET, SOCK_STREAM, 0);
    if (server_fd < 0)
    {
        die("Could not create socket\n");
    }

    server.sin_family = AF_INET;
    server.sin_port = htons(port);
    server.sin_addr.s_addr = htonl(INADDR_ANY);

    int opt_val = 1;
    setsockopt(server_fd, SOL_SOCKET, SO_REUSEADDR, &opt_val, sizeof opt_val);

    err = bind(server_fd, (struct sockaddr *) &server, sizeof(server));
    if (err < 0)
    {
        die("Could not bind socket\n");
    }

    err = listen(server_fd, 128);
    if (err < 0)
    {
        die("Could not listen on socket\n");
    }

    printf("Server is listening on %d\n", port);

    char* recv_buffer;
    size_t recv_buffer_size;
    while(true)
    {
        socklen_t client_len = sizeof(client);
        client_fd = accept(server_fd, (struct sockaddr *) &client, &client_len);

        if(client_fd < 0)
        {
            die("Could not establish new connection\n");
        }

        read_socket(client_fd, &recv_buffer, &recv_buffer_size);

        //*************************************************************
        //TODO: handle config better

        delaybind_session_config config = default_delaybind_session_config();
        config.client_port = 2080;
        config.remote_host = "104.236.88.125";
        config.remote_port = 6578;
        on_connect(config, recv_buffer, recv_buffer_size);
        //*************************************************************

        free(recv_buffer);
    }

    //cleanup global state
    ssh_finalize();
    return 0;
}
