{-# LANGUAGE OverloadedStrings, PostfixOperators #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-partial-type-signatures #-}
{-# OPTIONS_GHC -O0 -fno-cse -fno-full-laziness #-}  -- preserve "lexical" sharing for observed sharing
module Commands.Plugins.Spiros.Chrome.Gmail where 
import Commands.Plugins.Spiros.Shortcut.Types 
import Commands.Plugins.Spiros.Extra
-- import Commands.Plugins.Spiros.Chrome.Types 

import Commands.Mixins.DNS13OSX9


-- NOTE implicit dependency: enable with "Settings > Keyboard shortcuts > Keyboard shortcuts on." 
gmailShortcuts :: R Shortcut 
gmailShortcuts = shortcuts
 [ "compose mail"-: "c" 
 , "reply mail"-: "a"
 , "forward mail"-: "f"
 , "carbon copy"-: "M-S-c"
 , "blind carbon copy"-: "M-S-b"
 , "insert link"-: "C-k"
 , "bold"-: "M-b"
 , "italic"-: "M-i"
 , "underline"-: "M-u"
 , "send message"-: "M-<ret>"
 , "expand mail"-: ";"
 , "blur"-: "<esc>"             -- fade out from current input field, if any  

 , "Gmail search"-: "/"
 , "archive condo"-: "e"
 , "pick condo"-: "x"
 , "e-mail is read"-: "S-i"
 , "all mail"-: "g a"
 , "sent mail"-: "g t"
 , "inbox mail"-: "g i"
 , ""-: ""
 , ""-: ""
 , ""-: ""
 , ""-: ""
 , ""-: ""
 , ""-: ""
 , ""-: ""
 , ""-: ""
 , ""-: ""
 , ""-: ""
 , ""-: ""
 , ""-: ""
 , ""-: ""
 , ""-: ""
 , ""-: ""
 ]



{- enabled by default 
Mac: ⌘ + Enter
Send message	After composing your message, use this combination to send it.

Mac: ⌘ + .
Advance to next window	Use this shortcut to move the cursor to the next chat or compose window, or to the main window.

Mac: ⌘ + ,
Go to previous window	Use this shortcut to move the cursor to the previous chat or compose window, or to the main window.

Mac: ⌘ + Shift + c
Add Cc recipients	While composing, takes you to the Cc field to add new recipients.

Mac: ⌘ + Shift + b
Add Bcc recipients	While composing, takes you to the Bcc field to add new blind recipients.

Mac: ⌘ + Shift + f
Change "from" address	While composing, takes you to the From field to change your sending address. Thi
-}


{- formatting
Mac: ⌘ + Shift + +
Gmail Compose Size icon	Size - Increase the font size of all or some text

Mac: ⌘ + Shift + -
Gmail Compose Size icon	Size - Decrease the font size of all or some text

Mac: ⌘ + Shift + 5
Gmail Compose Formatting options icon	Font - Change font to the previous font as listed in the menu

Mac: ⌘ + Shift + 6
Gmail Compose Formatting options icon	Font - Change font to the next font as listed in the menu

Mac: ⌘ + b
Gmail Compose Bold icon	Bold - bold all or some of your message

Mac: ⌘ + i
Gmail Compose Italic icon	Italic - italicize all or some of your message

Mac: ⌘ + u
Gmail Compose Underline icon	Underline - underline all or some of your message

Gmail Compose Text Color icon	Text Color - change the color of all or some text, or of the text's background

Gmail Compose Numbered list icon	Numbered List - create a numbered list in your message
Mac: ⌘ + Shift + 7

Gmail Compose Bulleted List icon	Bulleted List - create a bulleted list in your message	Ctrl + Shift + 8
Mac: ⌘ + Shift + 8

Mac: ⌘ + \
Gmail Compose Remove Formatting icon	Remove Formatting - remove formatting from selected text

Mac: ⌘ + k
Gmail Compose Insert link icon	Insert link - hyperlink the selected text
-}


{- 

c
Compose	Allows you to compose a new message. Shift + c allows you to compose a message in a new window.

d
Compose in a new tab	Opens a compose window in a new tab.

/
Search	Puts your cursor in the search box.

k
Move to newer conversation	Opens or moves your cursor to a more recent conversation. You can hit Enter to expand a conversation.

j
Move to older conversation	Opens or moves your cursor to the next oldest conversation. You can hit Enter to expand a conversation.

n
Newer message	In 'Conversation view', moves your cursor to the newer message. You can hit Enter to expand or collapse a message.

p
Previous message	In 'Conversation view', moves your cursor to the older message. You can hit Enter to expand or collapse a message.

`
Go to next inbox section	If you use an inbox style with tabs or sections, you can quickly navigate to the next section.

~
Go to previous inbox section	If you use an inbox style with tabs or sections, you can quickly navigate to the previous section.

o or Enter
Open	Opens your conversation. Also expands or collapses a message if you are in 'Conversation View.'

u
Return to conversation list	Refreshes your page and returns you to the inbox, or list of conversations.

e
Archive	Archive your conversation from any view.

m
Mute	Archives the conversation, and all future messages skip the Inbox unless sent or cc'd directly to you. Learn more.

x
Select conversation	Automatically checks and selects a conversation so that you can archive, apply a label, or choose an action from the drop-down menu to apply to that conversation.

s
Star a message or conversation	Adds or removes a star to a message or conversation. Stars allow you to give a message or conversation a special status.

+
Mark as important	Helps Gmail learn what's important to you by marking misclassified messages. (Specific to Priority Inbox)

-
Mark as unimportant	Helps Gmail learn what's not important to you by marking misclassified messages. (Specific to Priority Inbox)

!
Report spam	Marks a message as spam and removes it from your conversation list.

r
Reply	Replies to the message sender. Shift + r allows you to reply to a message in a new window. (Only applicable in 'Conversation View.')

a
Reply all	Replies to all message recipients. Shift + a allows you to reply to all message recipients in a new window. (Only applicable in 'Conversation View.')

f
Forward	Forwards a message. Shift + f allows you to forward a message in a new window. (Only applicable in 'Conversation View.')

Esc
Escape from input field	Removes the cursor from your current input field.

Mac: ⌘ + s	Save draft	Saves the current text as a draft when composing a message. Hold the Ctrl/⌘ key while pressing s and make sure your cursor is in one of the text fields -- either the composition pane, or any of the To, CC, BCC, or Subject fields -- when using this shortcut.

#
Delete	Moves the conversation to Trash.

l
Label	Opens the Labels menu to label a conversation.

v
Move to	Moves the conversation from the inbox to a different label, Spam or Trash.

Shift + i
Mark as read	Marks your message as 'read' and skip to a newer message.

Shift + u
Mark as unread	Marks your message as 'unread' so you can go back to it later.

[
Removes from current view and previous	Removes the current view's label from your conversation and moves to the older one.

]
Removes from current view and next	Removes the current view's label from your conversation and moves to the newer one.

{
Archive and previous	Archives the current conversation and moves to the older one.

}
Archive and next	Archives the current conversation and moves to the next one.

z
Undo	Undoes your previous action, if possible (works for actions with an 'undo' link).

Shift + n
Update current conversation	Updates your current conversation when there are new messages.

q
Move cursor to chat search	Moves your cursor directly to the chat search box.

y
Remove from Current View*	Automatically removes the message or conversation from your current view.
From 'Inbox,' 'y' means Archive
From 'Starred,' 'y' means Unstar
From 'Trash,' 'y' means Move to inbox
From any label, 'y' means Remove the label
* 'y' has no effect if you're in 'Spam,' 'Sent,' or 'All Mail.'

.
Show more actions	Displays the 'More Actions' drop-down menu.

,
Moves cursor to the first button in your Gmail toolbar	Displays the 'More Actions' drop-down menu.

Mac: ⌘ + Down arrow	Opens options in Chat	

Ctrl/⌘ + Down arrow moves from edit field in your chat window to select the 'Video and more' menu
Next, press Tab to select the emoticon menu
Press Enter to open the selected menu

k
Move up a contact	Moves your cursor up in your contact list

j
Move down a contact	Moves your cursor down in your contact list

o or Enter
Open	Opens the contact with the cursor next to it.

u
Return to contact list view	Refreshes your page and returns you to the contact list.

e
Remove from Current Group	Removes selected contacts from the group currently being displayed.

x
Select contact	Checks and selects a contact so that you can change group membership or choose an action from the drop-down menu to apply to the contact.

Esc
Escape from input field	Removes the cursor from the current input

#
Delete	Deletes a contact permanently

l
Group membership	Opens the groups button to group contacts

z
Undo	Reverses your previous action, if possible (works for actions with an 'undo' link)  

-}


{- navigation 

Tab then Enter	Send message	After composing your message, use this combination to send it.
y then o	Archive and next	Archives your conversation and moves to the next one.
g then a	Go to 'All Mail'	Takes you to 'All Mail,' the storage site for all mail you've ever sent or received (and have not deleted).
g then s	Go to 'Starred'	Takes you to all conversations you have starred.
g then c	Go to 'Contacts'	Takes you to your Contacts list.
g then d	Go to 'Drafts'	Takes you to all drafts you have saved.
g then l	Go to 'Label'	Takes you to the search box with the "label:" operator filled in for you.
g then i	Go to 'Inbox'	Returns you to the inbox.
g then t	Go to 'Sent Mail'	Takes you to all mail you've sent.
* then a	Select all	Selects all mail.
* then n	Select none	Deselects all mail.
* then r	Select read	Selects all mail you've read.
* then u	Select unread	Selects all unread mail.
* then s	Select starred	Selects all starred mail.
* then t	Select unstarred	Selects all unstarred mail.

-} 

