#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>

#include <cstring>
#include <cerrno>

#include "network.hpp"

using namespace std;


/*
 * SystemCallException class
 */


static string * get_string_for_error(int errnum, const char *description)
{
    string *s = new string();
    *s += description;
    *s += ": ";
    *s += strerror(errnum);
    return s;
}

SystemCallException::SystemCallException(int errnum, const char *description)
    throw()
{
    _message = get_string_for_error(errnum, description);
}

SystemCallException::SystemCallException(int errnum, const string *description)
    throw()
{
    _message = get_string_for_error(errnum, description->c_str());
}

SystemCallException::SystemCallException(const char *message)
    throw()
{
    _message = new string(message);
}

SystemCallException::SystemCallException(const std::string *message)
    throw()
{
    _message = new string(*message);
}

const char * SystemCallException::what()
    const throw()
{
    return _message->c_str();
}

SystemCallException::~SystemCallException()
    throw()
{
    delete _message;
}


/*
 * Socket class
 */


Socket::Socket(const char *server, const char *port)
{
    int err;
    struct addrinfo *host_addrs;
    
    err = getaddrinfo(server, port, NULL, &host_addrs);
    if (err) {
        string message;
        message += "Error resolving network address: ";
        message += gai_strerror(err);
        throw new SystemCallException(&message);
    }
    
    // TODO: Can host_addrs be NULL? That would be awkward.
    
    _fd = socket(host_addrs->ai_family, host_addrs->ai_socktype,
                host_addrs->ai_protocol);
    if (_fd < 0) {
        throw new SystemCallException(errno, "Error creating socket");
    }
    
    err = connect(_fd, host_addrs->ai_addr, host_addrs->ai_addrlen);
    if (err < 0) {
        throw new SystemCallException(errno, "Error connecting socket");
    }
    
    freeaddrinfo(host_addrs);
}

Socket::~Socket() throw()
{
    close(_fd);
}

void Socket::send(const char *data, size_t nbytes)
{
    size_t total_bytes_written = 0;
    ssize_t bytes_written_this_iteration;
    
    while (total_bytes_written < nbytes) {
        bytes_written_this_iteration = write(_fd, data, nbytes -
                                             total_bytes_written);
        if (bytes_written_this_iteration < 0) {
            throw new SystemCallException(errno, "Write failed");
        }
        else {
            total_bytes_written += bytes_written_this_iteration;
            data += bytes_written_this_iteration;
        }
    }
}

void Socket::send(const char *data)
{
    send(data, strlen(data));
}

void Socket::send(const string *data)
{
    send(data->c_str(), data->size());
}

string Socket::receive(size_t max_length)
{
    char *buff = new char[max_length + 1];
    
    ssize_t num_bytes_read = read(_fd, buff, max_length);
    if (num_bytes_read < 0) {
        throw new SystemCallException(errno, "Read failed");
    }
    
    // Just to be absolutely sure our string is null-terminated.
    buff[max_length] = '\0';
    
    string ret(buff, num_bytes_read);
    delete [] buff;
    return ret;
}

