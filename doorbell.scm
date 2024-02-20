(define-module (doorbell)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 getopt-long)
  #:export (main))

;; TODO: Add input validation and feedback (irregex?)

(define (catch-all thunk)
  (with-exception-handler
      (lambda (exn)
        #f)
    thunk
    #:unwind? #t))

(define (dinger host port delay verbose?)
  (begin (display (string-append "dinging" host ":" port))
         (when verbose?
           (display (string-append "Knocking port: " port)))
         (let* ((s (socket PF_INET SOCK_STREAM 0))
                (flags (fcntl s F_GETFL)))
           ;; use nonblocking sockets
           (fcntl s F_SETFL (logior O_NONBLOCK flags))
           (connect s AF_INET (inet-pton AF_INET host) (string->number port))
           (when verbose?
             (display (string-append "Knocked port: " port)))
           (usleep (* 1000 (string->number delay)))
           ;; shutdown the socket for receiving and transmitting (2)
           ;; if too short of a delay socket transmission might fail?
           ;; untested.
           ;; NOTE: Consider making shutdown only for receiving
           (when verbose?
             (display (string-append "Delayed : " delay)))
           (close-port s))))

(define (ring-doorbell host ports delay verbose?)
  (for-each (lambda (p)
              (when verbose?
                (display (string-append "Ringing "
                                        host
                                        " doorbell ports: "
                                        ports
                                        " with delay of "
                                        delay)))
              (dinger host
                      p
                      delay
                      verbose?))
            (string-split ports #\,)))

(define help "\
doorbell is used to send packets in an endpoint in order with some time delay between packets
doorbell -h 192.168.1.1 -t 400,500,600 [options]
  -v,                    --version    Display version
                         --verbose    Display verbose information
                         --help       Display this help
  -t  port1,port2,portn  --tcp        Send comma separated tcp packets to provided port
  -h  host               --host       Specifies the host
  -d                     --delay      Delay in ms between packets sent. Default 100ms
  -e  cmd                --exec       Command to execute after packets sent")

(define (main args)
  (let* ((option-spec '((version (single-char #\v) (value #f))
                        (help    (value #f))
                        (host    (single-char #\h) (value #t))
                        (tcp     (single-char #\t) (value #t))
                        (delay   (single-char #\d) (value #t))
                        (verbose (value #f))
                        (exec    (single-char #\e) (value #t))))
         (options (getopt-long args option-spec))
         (help-wanted (option-ref options 'help #f))
         (delay (option-ref options 'delay #f))
         (tcp-port (option-ref options 'tcp #f))
         (host (option-ref options 'host #f))
         (exec (option-ref options 'exec #f))
         (verbose? (option-ref options 'verbose #f))
         (version-wanted (option-ref options 'version #f)))
    (newline)
    (if (or version-wanted
            help-wanted)
        ;;if version or help only show those
        (begin

          (if version-wanted
              (display "doorbell version 0.1\n"))
          (if help-wanted
              (display help)))

        ;; must have ports and host
        (if (and tcp-port host)
            (begin
              (ring-doorbell host
                             tcp-port
                             (if (nil? delay)
                                 100
                                 (string->number delay))
                             verbose?)
              (display (string-append "ringing the doorbell for " host)))
            (begin
              (display "Did not provide enough commands! Use --help for more info.") (newline))))
    (newline)))
