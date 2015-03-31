#ifndef CBITS_OBJC_ACTOR_H
#define CBITS_OBJC_ACTOR_H 1

#import <Cocoa/Cocoa.h>


const char * currentApplicationPath();
void pressKey(CGEventFlags modifiers, CGKeyCode key);
const char* getClipboard();
void setClipboard(const char* contents);
void openURL(const char* url);


#endif
