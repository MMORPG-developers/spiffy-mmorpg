#ifndef _NETWORK_HPP_INCLUDED
#define _NETWORK_HPP_INCLUDED

#include <string>
#include <exception>


// Custom exception class -- should probably be moved to its own file.
// Alternatively, we could #include <system_error>, but that requires the
// new C++11 standard, which is apparently experimental, at least in GCC.

class SystemCallException : public std::exception {
public:
    // Creates an exception object for the specified value of errno.
    SystemCallException(int errnum, const std::string *description) throw();
    SystemCallException(int errnum, const char *description) throw();
    
    // Creates an exception object with a custom error message.
    // Useful for functions that don't use the same error codes as everyone
    // else, like getaddrinfo.
    SystemCallException(const std::string *message) throw();
    SystemCallException(const char *message) throw();
    
    virtual const char *what() const throw();
    
    virtual ~SystemCallException() throw();

private:
    std::string *_message;
};



// Socket class -- a wrapper around the C library's socket functions that
// throws exceptions instead of returning -1 and setting errno.

class Socket {
public:
    Socket(const char *server, const char *port);
    ~Socket() throw();
    
    // Write data to the socket.
    void send(const char *data, size_t nbytes);
    void send(const char *data);
    void send(const std::string *data);
    
    // Read at most max_length bytes from the socket and return the result as
    // a std::string.
    std::string receive(size_t max_length);

private:
    int _fd;
};

#endif // _NETWORK_HPP_INCLUDED

