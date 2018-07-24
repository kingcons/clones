sbcl --eval "(ql:quickload :clones)" --eval "(in-package :clones)" \
     --eval "(change-game \"roms/commercial/smb.nes\")" \
     --eval "(require 'sb-sprof)" \
     --eval "(sb-sprof:profile-call-counts \"CLONES\" \"CLONES.CPU\" \"CLONES.MEMORY\")" \
     --eval "(sb-sprof:profile-call-counts \"CLONES.INSTRUCTIONS\" \"CLONES.PPU\")" \
     --eval "(sb-sprof:with-profiling (:max-samples 5000 :report :flat) (step-frames 240))"
#    --eval "(sb-sprof:with-profiling (:max-samples 5000 :mode :alloc :report :flat) (step-frames 240))"
