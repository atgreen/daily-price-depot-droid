all:
	@echo "Supported targets:"
	@echo " clean		- clean the source tree"
	@echo " podman-start	- run in podman using test/test-pod.yml"
	@echo " podman-stop	- stop the test pod"
	@echo " run		- run locally"

run:
	sbcl --eval '(setf ocicl-runtime:*download* t)' --eval '(asdf:load-system :daily-price-depot-droid)' --eval '(daily-price-depot-droid:start-server)'

podman-start:
	sh test/podman-start.sh

podman-stop:
	-podman pod stop daily-price-depot-droid-pod
	-podman pod rm daily-price-depot-droid-pod

clean:
	@rm -rf system-index.txt *~
