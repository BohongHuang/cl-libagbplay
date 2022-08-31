(defpackage cl-libagbplay
  (:use #:cl #:cffi)
  (:nicknames #:libagbplay)
  (:export
   #:agb-player
   #:agb-player-delete
   #:agb-player-play
   #:agb-player-pause
   #:agb-player-stop
   #:agb-player-playing-p
   #:agb-player-set-song
   #:agb-player-get-song-number
   #:agb-player-take-buffer
   #:make-agb-player))

(in-package #:libagbplay)

(define-foreign-library libagbplay
  (:darwin "libagbplay.dylib")
  (:unix "libagbplay.so")
  (:windows "libagbplay.dll")
  (t (:default "libagbplay")))

(unless (foreign-library-loaded-p 'libagbplay)
  (let ((*foreign-library-directories* '(".")))
    (use-foreign-library libagbplay)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmethod translate-name-from-foreign ((spec string) (package (eql *package*)) &optional varp)
    (let ((name (translate-camelcase-name spec :upper-initial-p t)))
      (if varp (intern (format nil "*~a" name)) name)))

  (defmethod translate-name-to-foreign ((spec symbol) (package (eql *package*)) &optional varp)
    (let ((name (translate-camelcase-name spec :upper-initial-p t)))
      (if varp (subseq name 1 (1- (length name))) name))))

(defctype size-t #.(cond ((= 4 (foreign-type-size :pointer)) :uint32)
                         ((= 8 (foreign-type-size :pointer)) :uint64)
                         (t (error "Failed type lisp-pointer-type"))))

(defcstruct agb-player
  (handle :pointer))

(defcstruct (%agb-player-config :class agb-player-config-type)
  (buffer-size size-t)
  (max-loop-count :int8)
  (verification-enabled :bool))

(defstruct agb-player-config
  max-loop-count buffer-size verification-enabled)

(defmethod translate-into-foreign-memory (object (type agb-player-config-type) pointer)
  (with-foreign-slots ((buffer-size max-loop-count) pointer (:struct %agb-player-config))
    (setf buffer-size (coerce (agb-player-config-buffer-size object) 'unsigned-byte))
    (setf max-loop-count (coerce (agb-player-config-max-loop-count object) '(signed-byte 8)))
    (setf verification-enabled (agb-player-config-verification-enabled object))))

(defmethod translate-from-foreign (pointer (type agb-player-config-type))
  (with-foreign-slots ((buffer-size max-loop-count verification-enabled) pointer (:struct %agb-player-config))
    (make-agb-player-config :buffer-size buffer-size
                            :max-loop-count max-loop-count
                            :verification-enabled verification-enabled)))

(defcfun "AgbPlayerCreateFromRomData" (:struct agb-player)
  (data :pointer)
  (size :uint)
  (config (:struct %agb-player-config)))

(defcfun "AgbPlayerCreateFromPath" (:struct agb-player)
  (path :string)
  (config (:struct %agb-player-config)))

(defgeneric make-agb-player (object &key max-loop-count buffer-size))

(defmethod make-agb-player ((path pathname) &key (max-loop-count -1) (buffer-size 8192) (verification t))
  (agb-player-create-from-path (namestring path)
                               (make-agb-player-config :max-loop-count max-loop-count
                                                       :buffer-size buffer-size
                                                       :verification-enabled verification)))

(defmethod make-agb-player ((rom vector) &key (max-loop-count -1) (buffer-size 8192) (verification t))
  (with-pointer-to-vector-data (rom-pointer rom)
    (agb-player-create-from-rom-data rom-pointer
                                     (length rom)
                                     (make-agb-player-config :max-loop-count max-loop-count
                                                             :buffer-size buffer-size
                                                             :verification-enabled verification))))

(defcfun "AgbPlayerDelete" :void
  (player (:struct agb-player)))

(defcfun "AgbPlayerPlay" :void
  (player (:struct agb-player)))

(defcfun "AgbPlayerPause" :void
  (player (:struct agb-player)))

(defcfun "AgbPlayerStop" :void
  (player (:struct agb-player)))

(defcfun "AgbPlayerIsPlaying" :void
  (player (:struct agb-player)))

(defun agb-player-playing-p (player)
  (agb-player-playing-p player))

(defcfun "AgbPlayerSetSong" :void
  (player (:struct agb-player))
  (song-id :uint16))

(defcfun "AgbPlayerGetSongNumber" :uint
  (player (:struct agb-player)))

(defcfun "AgbPlayerTakeBuffer" :void
  (player (:struct agb-player))
  (buffer :pointer)
  (size size-t))
