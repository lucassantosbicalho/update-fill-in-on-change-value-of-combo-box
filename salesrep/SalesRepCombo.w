&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

create widget-pool.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
define variable c-rep-code as character no-undo.
define variable c-rep-name as character no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS cb-cod 
&Scoped-Define DISPLAYED-OBJECTS cb-cod fill-name 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
define var C-Win as widget-handle no-undo.

/* Definitions of the field level widgets                               */
define variable cb-cod as character format "X(256)":U 
     view-as combo-box inner-lines 5
     drop-down-list
     size 16 by 1 no-undo.

define variable fill-name as character format "X(256)":U 
     view-as fill-in 
     size 45 by 1
     bgcolor 8  no-undo.


/* ************************  Frame Definitions  *********************** */

define frame DEFAULT-FRAME
     cb-cod at row 3.38 col 6 colon-aligned no-label widget-id 2
     fill-name at row 3.38 col 24 colon-aligned no-label widget-id 4
    with 1 down no-box keep-tab-order overlay 
         side-labels no-underline three-d 
         at col 1 row 1
         size 80 by 16 widget-id 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
if session:display-type = "GUI":U then
  create window C-Win assign
         hidden             = yes
         title              = "<insert window title>"
         height             = 16
         width              = 80
         max-height         = 16
         max-width          = 80
         virtual-height     = 16
         virtual-width      = 80
         resize             = yes
         scroll-bars        = no
         status-area        = no
         bgcolor            = ?
         fgcolor            = ?
         keep-frame-z-order = yes
         three-d            = yes
         message-area       = no
         sensitive          = yes.
else {&WINDOW-NAME} = current-window.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* SETTINGS FOR FILL-IN fill-name IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
if session:display-type = "GUI":U and VALID-HANDLE(C-Win)
then C-Win:hidden = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
on end-error of C-Win /* <insert window title> */
or endkey of {&WINDOW-NAME} anywhere do:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  if this-procedure:persistent then return no-apply.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
on window-close of C-Win /* <insert window title> */
do:
  /* This event will close the window and terminate the procedure.  */
  apply "CLOSE":U to this-procedure.
  return no-apply.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cb-cod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-cod C-Win
on value-changed of cb-cod in frame DEFAULT-FRAME
do:
    define variable c-cod as character no-undo.
    define variable c-name as character no-undo.
    
    assign
        c-cod  = entry(lookup(cb-cod:SCREEN-VALUE, cb-cod:LIST-ITEM-PAIRS) - 1, cb-cod:LIST-ITEM-PAIRS)
        c-name = entry(lookup(cb-cod:SCREEN-VALUE, cb-cod:LIST-ITEM-PAIRS),     cb-cod:LIST-ITEM-PAIRS)
        fill-name:screen-value in frame DEFAULT-FRAME = c-name.
    
    
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
assign CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
on close of this-procedure 
   run disable_UI.

/* Best default for GUI applications is...                              */
pause 0 before-hide.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
do on error   undo MAIN-BLOCK, leave MAIN-BLOCK
   on end-key undo MAIN-BLOCK, leave MAIN-BLOCK:
  run enable_UI.
 
  run prPopulateCombo.
  run prSetInitialValueCombo("DKP").
  
  if not this-procedure:persistent then
    wait-for close of this-procedure.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
procedure disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  if session:display-type = "GUI":U and VALID-HANDLE(C-Win)
  then delete widget C-Win.
  if this-procedure:persistent then delete procedure this-procedure.
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
procedure enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  display cb-cod fill-name 
      with frame DEFAULT-FRAME in window C-Win.
  enable cb-cod 
      with frame DEFAULT-FRAME in window C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  view C-Win.
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE prPopulateCombo C-Win 
procedure prPopulateCombo :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
for each SalesRep no-lock:
    c-rep-code = c-rep-code + "," 
                    + SalesRep.SalesRep + "," 
                    + replace(SalesRep.RepName, ", ", "").      
end.

assign 
    c-rep-code = trim (c-rep-code, ",")
    cb-cod:list-item-pairs in frame DEFAULT-FRAME = c-rep-code.

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE prSetInitialValueCombo C-Win
procedure prSetInitialValueCombo:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
define input  parameter ipSalesRep as character no-undo.
define variable iIndexSalesRep     as integer   no-undo.

assign 
    iIndexSalesRep = lookup(ipSalesRep, cb-cod:list-item-pairs in frame {&FRAME-NAME}).
    cb-cod:screen-value in frame {&FRAME-NAME} = entry(iIndexSalesRep + 1, c-rep-code). 
    apply "value-changed" to cb-cod.

end procedure.
    
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


