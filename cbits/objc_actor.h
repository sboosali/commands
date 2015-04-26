#ifndef CBITS_OBJC_ACTOR_H
#define CBITS_OBJC_ACTOR_H 1

#import <Cocoa/Cocoa.h>


ProcessSerialNumber* getApplicationPSN(const char* s);

const char * currentApplicationPath();
void bringApplication(const char* s);
void pressKey(CGEventFlags modifiers, CGKeyCode key);
void pressKeyTo(CGEventFlags modifiers, CGKeyCode key, ProcessSerialNumber psn);
const char* getClipboard();
void setClipboard(const char* contents);
void openURL(const char* url);


#endif
