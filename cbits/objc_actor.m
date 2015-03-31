#import <Cocoa/Cocoa.h>

#import "objc_actor.h"


// private helpers

NSString* fromUTF8(const char* s) {
 return [[NSString alloc]
          initWithCString:s encoding:NSUTF8StringEncoding];
}

ProcessSerialNumber currentApplicationPSN() {

  NSDictionary *appInfo = [[NSWorkspace sharedWorkspace] activeApplication];
  ProcessSerialNumber psn;
  psn.highLongOfPSN = [[appInfo objectForKey:@"NSApplicationProcessSerialNumberHigh"] unsignedIntValue];
  psn.lowLongOfPSN  = [[appInfo objectForKey:@"NSApplicationProcessSerialNumberLow"]  unsignedIntValue];

  return psn;
}


// public

const char* currentApplicationPath() {
 return [[[[NSWorkspace sharedWorkspace]
   activeApplication]
    objectForKey:@"NSApplicationPath"]
     cStringUsingEncoding:NSUTF8StringEncoding];
}

void pressKey(CGEventFlags modifiers, CGKeyCode key) {

    // events to press a key
    CGEventRef event1 = CGEventCreateKeyboardEvent(NULL, key, true);  // key down
    CGEventRef event2 = CGEventCreateKeyboardEvent(NULL, key, false); // key up

    // add modifiers to event
    CGEventSetFlags(event1, modifiers);
    CGEventSetFlags(event2, modifiers);

    // get the handle thing
    ProcessSerialNumber psn = currentApplicationPSN();

    // send keyboard event to application process (a quartz event)
    CGEventPostToPSN(&psn, event1);
    CGEventPostToPSN(&psn, event2);

    // release memory (do I need to?)
// event1
// event2
// psn

}

const char* getClipboard() {
 return [[[NSPasteboard generalPasteboard]
   stringForType:NSStringPboardType]
    cStringUsingEncoding:NSUTF8StringEncoding];
}

void setClipboard(const char* contents) {
 [[NSPasteboard generalPasteboard] clearContents];
 [[NSPasteboard generalPasteboard] setString:fromUTF8(contents) forType:NSStringPboardType];
}

void openURL(const char* url) {
 [[NSWorkspace sharedWorkspace]
   openURL:[NSURL URLWithString: fromUTF8(url)]];
}

