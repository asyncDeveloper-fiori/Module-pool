*&---------------------------------------------------------------------*
*& Modulpool ZMPP_HACK8_V1
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
PROGRAM zmpp_hack8_v1.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

"FCT_upload is the function code of the button
  IF sy-ucomm EQ 'FCT_UPLOAD'.

"data declarations required for the code to run various function modules

    data : p_file type rlgrap-filename.
    DATA : lv_email TYPE string,
           lv_desc  TYPE string,
           lv_file  TYPE string,
           lv_xstring type xstring,
           lt_binary_tab type table of x255,
           lt_solix_tab type solix_tab,
           lv_file_length type i,
           lv_filename type string.

"lv_email and lv_desc are 2 fields in module pool input op fields

    lv_email = lv_email.
    lv_desc = lv_desc.

"module for f4 f4 help to browse files

    CALL FUNCTION 'F4_FILENAME'
*     EXPORTING
*       PROGRAM_NAME        = sy-repid
*       DYNPRO_NUMBER       = SYST-DYNNR
*       FIELD_NAME          = ' '
     IMPORTING
       FILE_NAME           = p_file
              .

"as p_File is not compatibele with gui_upload we make another variable lv_filename of type string

lv_filename = p_file.

"function module to upload file

CALL FUNCTION 'GUI_UPLOAD'
  EXPORTING
    filename                      = lv_filename
   FILETYPE                      = 'BIN'
*   HAS_FIELD_SEPARATOR           = ' '
*   HEADER_LENGTH                 = 0
*   READ_BY_LINE                  = 'X'
*   DAT_MODE                      = ' '
*   CODEPAGE                      = ' '
*   IGNORE_CERR                   = ABAP_TRUE
*   REPLACEMENT                   = '#'
*   CHECK_BOM                     = ' '
*   VIRUS_SCAN_PROFILE            =
*   NO_AUTH_CHECK                 = ' '
 IMPORTING
   FILELENGTH                    = lv_file_length
*   HEADER                        =
  tables
    data_tab                      = lt_binary_tab
*
          .
IF sy-subrc <> 0.
* Implement suitable error handling here
ENDIF.


"convert binary data to xstring

      CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
        EXPORTING
          input_length       = lv_file_length
*         FIRST_LINE         = 0
*         LAST_LINE          = 0
       IMPORTING
         BUFFER             = lv_xstring
        tables
          binary_tab         = lt_binary_tab
*       E
                .
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.

" convert x string to binary as email attatchment only take binary data

      CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
        EXPORTING
          buffer                = lv_xstring
*         APPEND_TO_TABLE       = ' '
*       IMPORTING
*         OUTPUT_LENGTH         =
        tables
          binary_tab            = lt_solix_tab
                .

  ENDIF.

"FCT submit function module for submit button and logic below is for email with description and image attatched to it

  IF sy-ucomm EQ 'FCT_SUBMIT'.
*  email logic starts

    constants : gc_subject     type so_obj_des value 'Incident report',
                gc_text        type soli value '',
                gc_type_raw    type so_obj_tp value 'RAW',
                gc_att_type    type soodk-objtp value 'PNG',
                gc_att_subject type sood-objdes value 'Document in PNG'.

    DATA :
      gt_text           TYPE soli_tab,
      gt_attachment_hex TYPE solix_tab,
      gv_sent_to_all    TYPE os_boolean,
      gv_error_message  TYPE string,
      go_send_request   TYPE REF TO cl_bcs,
      go_recipient      TYPE REF TO if_recipient_bcs,
      go_sender         TYPE REF TO cl_Sapuser_bcs,
      go_document       TYPE REF TO cl_document_bcs,
      gc_email_to       TYPE adr6-smtp_addr,
      gx_bcs_exception  TYPE REF TO cx_bcs.

    TRY .
        gc_email_to = lv_email.
        go_send_request = cl_bcs=>create_persistent( ).

        go_sender = cl_sapuser_bcs=>create( sy-uname ).
        go_send_request->set_sender( i_sender = go_sender ).

        go_recipient = cl_cam_address_bcs=>create_internet_address( gc_email_to ).

        go_send_request->add_recipient(
        EXPORTING
          i_recipient = go_recipient
          i_express = abap_true ).


        SPLIT lv_desc AT cl_abap_char_utilities=>newline INTO TABLE gt_text.


*        email body
        APPEND gc_text TO gt_text.
        go_document = cl_document_bcs=>create_document(
        i_type = gc_type_raw
        i_text = gt_text
        i_length = '12'
        i_subject = gc_subject ).

*        attatchment
        go_document->add_attachment(
        EXPORTING
          i_attachment_type = gc_att_type
          i_attachment_subject = gc_att_subject
          i_att_content_hex =  lt_solix_tab ).

        go_send_request->set_document( go_document ).

        gv_sent_to_all = go_send_request->send( i_with_error_screen = abap_true ).

        IF gv_sent_to_all = abap_true.
          WRITE 'Email sent'.
        ENDIF.

        COMMIT WORK.

      CATCH cx_bcs INTO gx_bcs_exception.
        gv_error_message = gx_bcs_exception->get_text( ).
        WRITE gv_error_message.

    ENDTRY.
  ENDIF.
ENDMODULE.
