all:
	@echo "Supported targets:"
	@echo " clean		- clean the source tree"
	@echo " podman-start	- run in podman using test/test-pod.yml"
	@echo " podman-stop	- stop the test pod"
	@echo " run		- run locally"

run:
	sbcl --eval '(pushnew (truename "./src") ql:*local-project-directories* )' \
	     --eval '(ql:register-local-projects)' \
	     --eval '(ql:quickload :daily-price-depot-droid)' \
	     --eval '(daily-price-depot-droid:start-server)'

podman-start:
	sh test/podman-start.sh

podman-stop:
	-podman pod stop daily-price-depot-droid-pod
	-podman pod rm daily-price-depot-droid-pod

clean:
	@rm -rf system-index.txt *~
