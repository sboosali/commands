run: main
	./main

main: main.m objc_actor.m
	gcc -ObjC  -framework Cocoa  -o ./main  -Wno-deprecated-declarations  $^
# launchedApplications is deprecated, but runningApplications doesn't return application info

clean:
	rm -f main
