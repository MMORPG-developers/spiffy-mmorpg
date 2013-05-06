#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>

// FIXME: Shouldn't there be a C++ equivalent? Or can we at least use
// <cstring>? And likewise for all the other C-style <___.h> includes.
#include <string.h>
#include <stdlib.h>

// FIXME: #include'ing stdio.h and iostream in the same file? I see no way this
// can go wrong. Just like mixing perror, gai_strerror, and std::cout.
#include <stdio.h>
#include <iostream>

const char *SERVER = "localhost";
// The port is specified as a string containing its decimal representation, not
// as the number itself. Presumably because the port needs to be sent as a
// string over the network anyway, but still... seriously guys?
const char *PORT = "6667";

const size_t BUFFSIZE = 0x1000; // 4096

int main(void)
{
    char buff[BUFFSIZE];
    int err;
    
    struct addrinfo *host_addrs;
    err = getaddrinfo(SERVER, PORT, NULL, &host_addrs);
    if (err) {
        std::cout << "Error resolving network address: "
                  << gai_strerror(err)
                  << std::endl;
        exit(1);
    }
    
    // Can host_addrs be NULL? That would be awkward.
    
    int fd = socket(host_addrs->ai_family, host_addrs->ai_socktype,
                    host_addrs->ai_protocol);
    if (fd < 0) {
        perror("Error creating socket");
        exit(1);
    }
    
    err = connect(fd, host_addrs->ai_addr, host_addrs->ai_addrlen);
    if (err < 0) {
        perror("Error connecting socket");
        // If, for some reason, we don't exit here, then the later call to
        // write will cause this process to receive a SIGPIPE signal. Unless
        // we've specifically said that we'll catch that signal, the program
        // will exit without any sort of message at all, so we won't even get
        // a chance to say that the write failed.
        exit(1);
    }
    
    // This is pretty much the only function in this entire file that doesn't
    // return an error code.
    // Okay, that's not fair. perror and strncpy also don't return error codes.
    // But honestly, I think that's it.
    // (Except that I have no idea what operator<<(std::cout, ___) does if
    // problems happen, but since printf returns an error code, I'm guessing
    // that we're really supposed to be checking those calls for errors as
    // well).
    freeaddrinfo(host_addrs);
    
    // Copy the string to the buffer
    const char *const message = "Why hello there, Mr. server!\n";
    size_t messagelen = strlen(message);
    strncpy(buff, message, messagelen+1);
    
    // Apparently write returns an ssize_t (signed size_t?). Who knew?
    //
    // Also, it looks like Erlang doesn't like null-terminated strings.
    // The formatted output from the server shows a garbage character whenever
    // we put a null byte in. So maybe this should be messagelen instead of
    // messagelen+1?
    //
    // Oh, but it gets better. If you run the server with its input redirected
    // from a file (say, by running the script 'server' in the server
    // directory), then it doesn't print the garbage character regardless of
    // whether the client sends the null byte. I have no earthly idea why it
    // makes a difference, and I'm not at all sure I want to try to figure it
    // out.
    ssize_t num_bytes_written = write(fd, buff, messagelen+1);
    if (num_bytes_written < 0) {
        perror("Write failed");
        exit(1);
    }
    
    // In a happy turn of events, write returns an ssize_t while strlen returns
    // a size_t. The former is signed; the latter is unsigned. So if we want to
    // compare the number of bytes actually written by our write call to the
    // length of the string we were trying to write, we have to cast one of
    // them to the other's type (otherwise we get a warning).
    // But that's okay, because why would we ever want to do this? I mean after
    // all, the number of bytes in the original message and the number of bytes
    // written to the socket are completely different types and it doesn't make
    // sense to compare them.
    else if ((size_t)num_bytes_written < messagelen+1) {
        std::cout << "Warning: not all bytes written." << std::endl;
    }
    
    // Pass BUFFSIZE-1 so we are confident we can add a null byte to the end.
    ssize_t num_bytes_read = read(fd, buff, BUFFSIZE-1);
    if (num_bytes_read < 0) {
        perror("Read failed");
        exit(1);
    }
    
    // Just to be absolutely sure our string is null-terminated.
    buff[num_bytes_read] = '\0';
    
    // Guess which programming language I'd rather be using right now.
    // Hint: it's the only language I know that uses triple quotes.
    std::cout << "Received \"\"\""
              << buff
              << "\"\"\" from server."
              << std::endl;
    
    std::cout << "Yay, server/client communications work!" << std::endl;
    
    return 0;
}

