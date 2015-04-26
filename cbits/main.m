#import <Cocoa/Cocoa.h>
#import <Carbon/Carbon.h>

#import "objc_actor.h"

/* 
[NSThread sleepForTimeInterval:(30 / 1000.0f)];
  NSLog(@" count is %lu",[appInfos count]);
    NSLog(@" path is %@", [appInfo objectForKey:@"NSApplicationPath"]);

*/

int main(int argc, char** argv)
{
  // type command-shift-b, into current application
  // pressKey((kCGEventFlagMaskCommand | kCGEventFlagMaskControl), kVK_ANSI_B);

    // setClipboard(currentApplicationPath());
    // char * site  = "https://www.google.com/search?q=";
    // const char * query = getClipboard();
    // char * url = malloc(strlen(site) + strlen(query) + 1);
    // strcpy(url, site);
    // strcat(url, query);
    // openURL(url);

     // bringApplication("Notes");  // works, sometimes
     // when launched from another Emacs like Work, Notes inherits its environment variables
 // bringApplication("Google Chrome");  // works, no more weirdness

  printApplications ();

 // full path (name would be "Emacs")
  ProcessSerialNumber* psn1 = getApplicationPSN("/Applications/Notes.app");
  pressKeyTo(0, kVK_ANSI_X, *psn1);

[NSThread sleepForTimeInterval:(30 / 1000.0f)]; //  otherwise, the next event is lost

 // just name
  ProcessSerialNumber* psn2 = getApplicationPSN("Google Chrome");
  pressKeyTo(0, kVK_ANSI_X, *psn2);
 
  //  doesn't crash
  ProcessSerialNumber* psn3 = getApplicationPSN("doesn't exist");
  pressKeyTo(0, kVK_ANSI_X, *psn3);



  // double-click the mouse while holding command, at current location
  // [Actor clickMouse:kCGEventLeftMouseDown and:kCGEventLeftMouseUp on:kCGMouseButtonLeft for:1 with:0];

 // nope: caused weirdness
 //  [[NSWorkspace sharedWorkspace] launchApplication: @"Google Chrome"];

 // nope: didn't work
 // [[NSWorkspace sharedWorkspace] openFile:@"~/Dropbox/NOW" withApplication:@"Notes.app"];

  return 0;
}
