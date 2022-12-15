*&---------------------------------------------------------------------*
*& Report  Z_TRANSPORTE_COPIA
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_transporte_copia.

TYPE-POOLS: sabc.

TYPE-POOLS: trwbo, abap.

DATA: v_trkorr TYPE e070-trkorr.
DATA: v_error."  TYPE abap_bool.

PARAMETERS: p_trkorr TYPE trkorr OBLIGATORY.
PARAMETERS: p_transp TYPE c AS CHECKBOX DEFAULT 'X'.
PARAMETERS: p_downl  TYPE c AS CHECKBOX DEFAULT ' '.
PARAMETERS: p_sepr   TYPE c LENGTH 1 OBLIGATORY.
PARAMETERS: p_folder(255) TYPE c LOWER CASE DEFAULT 'C:\Temp\SAPReq'.

INITIALIZATION.
  CONCATENATE sy-sysid 'K*' INTO p_trkorr.
*  CALL FUNCTION 'WSAF_BUILD_SEPARATOR'
*    IMPORTING
*      separator                  = p_sepr
*    EXCEPTIONS
*      separator_not_maintained   = 1
*      wrong_call                 = 2
*      wsaf_config_not_maintained = 3
*      OTHERS                     = 4.
*  IF sy-subrc NE 0.
*    MESSAGE s001(00) WITH 'Unable to find out the separator symbol for the system.'(011).
*  ENDIF.
  p_sepr = '/'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_trkorr.
  PERFORM f_trkorr_search_help.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_folder.
  PERFORM f_folder_search_help.

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF screen-name CP '*P_SEPR*'   OR
       screen-name CP '*P_FOLDER*' OR
       screen-name CP '*P_DOWNL*'  OR
       screen-name CP '*P_TRANSP*' .
      screen-input = '0'.
      screen-invisible = '1'.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

START-OF-SELECTION.
*  PERFORM f_release_tasks.
  PERFORM f_create_copy_tr.
  PERFORM f_copy_objects.
  PERFORM f_unlock_original.
  PERFORM f_lock_copy_tr.
  PERFORM f_release_copy_tr.
  PERFORM f_lock_original.
  PERFORM f_show_transport_log.

*&---------------------------------------------------------------------*
*&      Form  F_CREATE_COPY_TR
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM f_create_copy_tr .


  DATA: lw_trwbo_request TYPE trwbo_request."trwbo_request.
  DATA: lv_shorttext     TYPE e07t-as4text.

  CLEAR lw_trwbo_request.

  SHIFT p_trkorr LEFT DELETING LEADING space.

  CALL FUNCTION 'TR_READ_REQUEST'
    EXPORTING
      iv_read_e07t     = 'X'
      iv_trkorr        = p_trkorr
    CHANGING
      cs_request       = lw_trwbo_request
    EXCEPTIONS
      error_occured    = 1
      no_authorization = 2
      OTHERS           = 3.

  IF sy-subrc IS NOT INITIAL.
    MESSAGE ID sy-msgid
          TYPE sy-msgty
        NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    RETURN.
  ENDIF.

  lv_shorttext = lw_trwbo_request-h-as4text.
  TRY .
      SPLIT lv_shorttext AT '-' INTO DATA(v_prj) DATA(v_tip) DATA(v_mod) DATA(v_desc).

      v_tip = 'T'.

      CLEAR lv_shorttext.

      lv_shorttext = v_prj && '-' && v_tip && '-' && v_mod && '-' && v_desc.

    CATCH cx_root.

  ENDTRY.




  CLEAR v_trkorr.
  CALL FUNCTION 'TRINT_INSERT_NEW_COMM'
    EXPORTING
      wi_kurztext       = lv_shorttext
      wi_trfunction     = 'T'
      iv_username       = sy-uname
      iv_tarsystem      = '/GROUP/'
      wi_client         = sy-mandt
    IMPORTING
      we_trkorr         = v_trkorr
    EXCEPTIONS
      no_systemname     = 1
      no_systemtype     = 2
      no_authorization  = 3
      db_access_error   = 4
      file_access_error = 5
      enqueue_error     = 6
      number_range_full = 7
      invalid_input     = 8
      OTHERS            = 9.

  IF sy-subrc IS NOT INITIAL.
    MESSAGE ID sy-msgid
          TYPE sy-msgty
        NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    RETURN.
  ENDIF.

ENDFORM.                    " F_CREATE_COPY_TR

*&---------------------------------------------------------------------*
*&      Form  F_TRKORR_SEARCH_HELP
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM f_trkorr_search_help .

  DATA: lw_selected_request TYPE trwbo_request_header.
  DATA: lw_selected_task TYPE trwbo_request_header.
  DATA: lv_organizer_type TYPE trwbo_calling_organizer.
  DATA: lw_selection TYPE trwbo_selection.

  lv_organizer_type = 'W'.
  lw_selection-reqstatus = 'R'.

  CLEAR lw_selected_request.
  CLEAR lw_selected_task.
  CALL FUNCTION 'TR_PRESENT_REQUESTS_SEL_POPUP'
    EXPORTING
      iv_organizer_type   = lv_organizer_type
      is_selection        = lw_selection
    IMPORTING
      es_selected_request = lw_selected_request
      es_selected_task    = lw_selected_task.

  p_trkorr = lw_selected_request-trkorr.

ENDFORM.                    " F_TRKORR_SEARCH_HELP

*&---------------------------------------------------------------------*
*&      Form  F_RELEASE_TASKS
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM f_release_tasks .

  DATA: lt_header TYPE trwbo_request_headers.
  DATA: lt_requests TYPE trwbo_requests.
  DATA: lw_trwbo_request TYPE trwbo_request.

  FIELD-SYMBOLS: <lf_header> LIKE LINE OF lt_header.

  REFRESH lt_header.
  REFRESH lt_requests.
  CALL FUNCTION 'TR_READ_REQUEST_WITH_TASKS'
    EXPORTING
      iv_trkorr          = p_trkorr
    IMPORTING
      et_request_headers = lt_header
      et_requests        = lt_requests
    EXCEPTIONS
      invalid_input      = 1
      OTHERS             = 2.

  IF sy-subrc IS NOT INITIAL.
    MESSAGE ID sy-msgid
          TYPE sy-msgty
        NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    RETURN.
  ENDIF.

  LOOP AT lt_header ASSIGNING <lf_header> WHERE ( trfunction EQ 'S' OR trfunction EQ 'R' )
                                            AND trstatus   EQ 'D'.

    CALL FUNCTION 'TR_RELEASE_REQUEST'
      EXPORTING
        iv_trkorr                  = <lf_header>-trkorr
        iv_dialog                  = ' '
        iv_success_message         = ' '
        iv_display_export_log      = ' '
        iv_as_background_job       = ' '
      EXCEPTIONS
        cts_initialization_failure = 1
        enqueue_failed             = 2
        no_authorization           = 3
        invalid_request            = 4
        request_already_released   = 5
        repeat_too_early           = 6
        error_in_export_methods    = 7
        object_check_error         = 8
        docu_missing               = 9
        db_access_error            = 10
        action_aborted_by_user     = 11
        export_failed              = 12
        OTHERS                     = 13.

    IF sy-subrc EQ 8.
      MESSAGE e398(00) WITH 'Erro ao liberar task. Objetos inativos.'.
      v_error = abap_true.
      RETURN.
    ENDIF.

    DO.
      CLEAR lw_trwbo_request.
      CALL FUNCTION 'TR_READ_REQUEST'
        EXPORTING
          iv_read_e070     = 'X'
          iv_trkorr        = <lf_header>-trkorr
        CHANGING
          cs_request       = lw_trwbo_request
        EXCEPTIONS
          error_occured    = 1
          no_authorization = 2
          OTHERS           = 3.

      IF sy-subrc IS NOT INITIAL.
        MESSAGE e398(00) WITH 'Erro ao liberar task'.
        v_error = abap_true.
        RETURN.
      ELSE.
        IF lw_trwbo_request-h-trstatus EQ 'R'.
          v_error = abap_false.
          EXIT.
        ENDIF.
      ENDIF.
    ENDDO.
  ENDLOOP.

ENDFORM.                    " F_RELEASE_TASKS

*&---------------------------------------------------------------------*
*&      Form  F_COPY_OBJECTS
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM f_copy_objects .

  CALL FUNCTION 'TR_COPY_COMM'
    EXPORTING
      wi_dialog                = 'X'
      wi_trkorr_from           = p_trkorr
      wi_trkorr_to             = v_trkorr
      wi_without_documentation = 'X'
    EXCEPTIONS
      OTHERS                   = 1.

ENDFORM.                    " F_COPY_OBJECTS

*&---------------------------------------------------------------------*
*&      Form  F_RELEASE_COPY_TR
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM f_release_copy_tr .

  DATA: lw_trwbo_request TYPE trwbo_request.

  CALL FUNCTION 'TR_RELEASE_REQUEST'
    EXPORTING
      iv_trkorr             = v_trkorr
      iv_dialog             = 'X'
      iv_success_message    = ' '
      iv_display_export_log = ' '
      iv_as_background_job  = ' '
    EXCEPTIONS
      OTHERS                = 0.

  DO.
    CLEAR lw_trwbo_request.
    CALL FUNCTION 'TR_READ_REQUEST'
      EXPORTING
        iv_read_e070     = 'X'
        iv_trkorr        = v_trkorr
      CHANGING
        cs_request       = lw_trwbo_request
      EXCEPTIONS
        error_occured    = 1
        no_authorization = 2
        OTHERS           = 3.

    IF sy-subrc IS NOT INITIAL.
      MESSAGE e398(00) WITH 'Erro ao liberar request transp. copias'.
      v_error = abap_true.
      RETURN.
    ELSE.
      IF lw_trwbo_request-h-trstatus EQ 'R'.
        EXIT.
      ENDIF.
    ENDIF.
  ENDDO.

ENDFORM.                    " F_RELEASE_COPY_TR

*&---------------------------------------------------------------------*
*&      Form  F_START_IMPORT
*&---------------------------------------------------------------------*
*

*&---------------------------------------------------------------------*
*&      Form  F_SHOW_TRANSPORT_LOG
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM f_show_transport_log .

  CALL FUNCTION 'TMS_UI_SHOW_TRANSPORT_LOGS'
    EXPORTING
      iv_request                 = v_trkorr
      iv_system                  = 'S4Q'
    EXCEPTIONS
      show_transport_logs_failed = 1
      OTHERS                     = 2.

  IF sy-subrc IS NOT INITIAL.
    RETURN.
  ENDIF.

ENDFORM.                    " F_SHOW_TRANSPORT_LOG

*&---------------------------------------------------------------------*
*&      Form  F_UNLOCK_ORIGINAL
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM f_unlock_original .

  DATA: lv_removed_locks TYPE i.
  DATA: lw_trwbo_request TYPE trwbo_request.

  CLEAR lw_trwbo_request.
  CALL FUNCTION 'TR_READ_REQUEST'
    EXPORTING
      iv_read_e070     = 'X'
      iv_trkorr        = p_trkorr
    CHANGING
      cs_request       = lw_trwbo_request
    EXCEPTIONS
      error_occured    = 1
      no_authorization = 2
      OTHERS           = 3.

  IF sy-subrc IS NOT INITIAL.
    MESSAGE ID sy-msgid
          TYPE sy-msgty
        NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    v_error = abap_true.
    RETURN.
  ENDIF.

  CALL FUNCTION 'TRINT_UNLOCK_REQUEST'
    EXPORTING
      iv_safe_mode           = ' '
      iv_set_to_released     = ' '
    IMPORTING
      ev_cnt_removed_locks   = lv_removed_locks
    CHANGING
      cs_request             = lw_trwbo_request
    EXCEPTIONS
      db_access_error        = 1
      object_enqueues        = 2
      no_authority           = 3
      request_not_changeable = 4
      OTHERS                 = 5.

  IF sy-subrc IS NOT INITIAL.
    MESSAGE ID sy-msgid
          TYPE sy-msgty
        NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    v_error = abap_true.
    RETURN.
  ENDIF.

ENDFORM.                    " F_UNLOCK_ORIGINAL

*&---------------------------------------------------------------------*
*&      Form  F_LOCK_COPY_TR
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM f_lock_copy_tr .

  CALL FUNCTION 'TR_LOCK_REQUEST'
    EXPORTING
      iv_trkorr          = v_trkorr
      iv_success_message = ' '
      iv_dialog          = ' '
    EXCEPTIONS
      error_occured      = 1
      wrong_call         = 2
      OTHERS             = 3.

  IF sy-subrc IS NOT INITIAL.
    MESSAGE ID sy-msgid
          TYPE sy-msgty
        NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    v_error = abap_true.
    RETURN.
  ENDIF.

ENDFORM.                    " F_LOCK_COPY_TR

*&---------------------------------------------------------------------*
*&      Form  F_LOCK_ORIGINAL
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM f_lock_original .

  CALL FUNCTION 'TR_LOCK_REQUEST'
    EXPORTING
      iv_trkorr          = p_trkorr
      iv_success_message = ' '
      iv_dialog          = ' '
    EXCEPTIONS
      error_occured      = 1
      wrong_call         = 2
      OTHERS             = 3.

  IF sy-subrc IS NOT INITIAL.
    MESSAGE ID sy-msgid
          TYPE sy-msgty
        NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    v_error = abap_true.
    RETURN.
  ENDIF.

ENDFORM.                    " F_LOCK_ORIGINAL

*&---------------------------------------------------------------------*
*&      Form  F_DOWNLOAD_FILES
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM f_download_files .

  DATA: lv_trfile TYPE c LENGTH 11.

  CONCATENATE v_trkorr+3(7) '.' v_trkorr(3) INTO lv_trfile.

  PERFORM f_copy_file USING 'cofiles' lv_trfile.
  lv_trfile(1) = 'R'.
  PERFORM f_copy_file USING 'data' lv_trfile.
  lv_trfile(1) = 'D'.
  PERFORM f_copy_file USING 'data' lv_trfile.

ENDFORM.                    " F_DOWNLOAD_FILES

*&---------------------------------------------------------------------*
*&      Form  f_copy_file
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SUBDIR   text
*      -->P_FNAME    text
*----------------------------------------------------------------------*
FORM f_copy_file USING p_subdir p_fname.
  DATA: lv_gui_filename TYPE string.
  DATA: lv_transdir TYPE text255.
  DATA: lt_datatab TYPE TABLE OF text8192 WITH HEADER LINE.
  DATA: lv_flen TYPE i.
  DATA: lv_filename LIKE authb-filename.
  DATA: lv_len TYPE i.

  CLEAR lv_transdir.
  CALL FUNCTION 'RSPO_R_SAPGPARAM'
    EXPORTING
      name   = 'DIR_TRANS'
    IMPORTING
      value  = lv_transdir
    EXCEPTIONS
      error  = 0
      OTHERS = 0.

  CONCATENATE lv_transdir p_subdir p_fname INTO lv_filename SEPARATED BY p_sepr.

  REFRESH lt_datatab.
  CLEAR lv_flen.
  CALL FUNCTION 'AUTHORITY_CHECK_DATASET'
    EXPORTING
      activity         = sabc_act_read
      filename         = lv_filename
    EXCEPTIONS
      no_authority     = 1
      activity_unknown = 2
      OTHERS           = 3.

  IF sy-subrc <> 0.
    FORMAT COLOR COL_NEGATIVE.
    WRITE: / 'Read access denied. File'(001), lv_filename.
    FORMAT COLOR OFF.
    EXIT.
  ENDIF.
  OPEN DATASET lv_filename FOR INPUT IN BINARY MODE.
  IF sy-subrc NE 0.
    FORMAT COLOR COL_TOTAL.
    WRITE: / 'File open error'(010), lv_filename.
    FORMAT COLOR OFF.
    EXIT.
  ENDIF.
  DO.
    CLEAR lv_len.
    READ DATASET lv_filename INTO lt_datatab LENGTH lv_len.
    lv_flen = lv_flen + lv_len.
    IF lv_len > 0.
      APPEND lt_datatab.
    ENDIF.
    IF sy-subrc NE 0.
      EXIT.
    ENDIF.
  ENDDO.
  CLOSE DATASET lv_filename.
  CONCATENATE p_folder '\' p_fname INTO lv_gui_filename.
  CALL METHOD cl_gui_frontend_services=>gui_download
    EXPORTING
      bin_filesize            = lv_flen
      filename                = lv_gui_filename
      filetype                = 'BIN'
    CHANGING
      data_tab                = lt_datatab[]
    EXCEPTIONS
      file_write_error        = 1
      no_batch                = 2
      gui_refuse_filetransfer = 3
      invalid_type            = 4
      no_authority            = 5
      unknown_error           = 6
      header_not_allowed      = 7
      separator_not_allowed   = 8
      filesize_not_allowed    = 9
      header_too_long         = 10
      dp_error_create         = 11
      dp_error_send           = 12
      dp_error_write          = 13
      unknown_dp_error        = 14
      access_denied           = 15
      dp_out_of_memory        = 16
      disk_full               = 17
      dp_timeout              = 18
      file_not_found          = 19
      dataprovider_exception  = 20
      control_flush_error     = 21
      not_supported_by_gui    = 22
      error_no_gui            = 23
      OTHERS                  = 24.
  IF sy-subrc = 0.
    WRITE: / 'File'(002), lv_filename, 'downloaded. Length'(003), lv_flen.
  ELSE.
    FORMAT COLOR COL_NEGATIVE.
    WRITE: / 'File download error. Filename:'(004), lv_filename.
    FORMAT COLOR OFF.
  ENDIF.
ENDFORM. "copy_file

*&---------------------------------------------------------------------*
*&      Form  F_FOLDER_SEARCH_HELP
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM f_folder_search_help .

  DATA: lv_title TYPE string.
  DATA: lv_folder TYPE string.

  lv_title = 'Select target folder'(005).
  CLEAR lv_folder.
  CALL METHOD cl_gui_frontend_services=>directory_browse
    EXPORTING
      window_title    = lv_title
    CHANGING
      selected_folder = lv_folder
    EXCEPTIONS
      cntl_error      = 1
      error_no_gui    = 2
      OTHERS          = 3.

  CALL FUNCTION 'CONTROL_FLUSH'
    EXCEPTIONS
      cntl_system_error = 1
      cntl_error        = 2
      OTHERS            = 3.

  p_folder = lv_folder.
ENDFORM.                    " F_FOLDER_SEARCH_HELP