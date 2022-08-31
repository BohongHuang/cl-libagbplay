(defpackage cl-libagbplay-example
  (:use #:cl #:libagbplay #:raylib)
  (:import-from #:cffi #:make-shareable-byte-vector #:with-pointer-to-vector-data)
  (:nicknames #:libagbplay-example)
  (:export #:run-example))

(in-package #:libagbplay-example)

(defparameter +buffer-size+ 4096)

(defun run-example ()
  (with-window (800 450 "cl-libagbplay example")
    (let* ((player (make-agb-player #P"rom.gba" :verification nil))
           (buffer (make-shareable-byte-vector (* +buffer-size+ 4 2)))
           (song-id 0)
           (song-count (agb-player-get-song-number player)))
      (set-target-fps 30)
      (set-audio-stream-buffer-size-default +buffer-size+)
      (with-audio-device
        (with-audio-stream (stream 48000 32 2)
          (unwind-protect
               (progn
                 (agb-player-set-song player song-id)
                 (agb-player-play player)
                 (play-audio-stream stream)
                 (loop :until (window-should-close)
                       :do (progn
                             (with-pointer-to-vector-data (buffer-pointer buffer)
                               (when (is-audio-stream-processed stream)
                                 (agb-player-take-buffer player buffer-pointer +buffer-size+)
                                 (update-audio-stream stream buffer-pointer +buffer-size+)))
                             (with-drawing
                               (when (cond
                                       ((is-key-pressed +key-up+) (decf song-id))
                                       ((is-key-pressed +key-down+) (incf song-id))
                                       ((is-key-pressed +key-left+) (decf song-id 10))
                                       ((is-key-pressed +key-right+) (incf song-id 10))
                                       ((is-key-pressed +key-space+) (agb-player-pause player))
                                       (t nil))
                                 (agb-player-set-song player (setf song-id (mod song-id song-count))))
                               (clear-background +raywhite+)
                               (draw-fps 0 0)
                               (draw-text (format nil "Current Song: ~A" song-id) 50 50 32 +black+)))))
            (agb-player-stop player)
            (with-pointer-to-vector-data (buffer-pointer buffer)
              (agb-player-take-buffer player buffer-pointer +buffer-size+))
            (agb-player-delete player)
            (stop-audio-stream stream)))))))
