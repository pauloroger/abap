*&---------------------------------------------------------------------*
*& Report  ZCUT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT  ZCUT.
DATA: itab(200) TYPE c OCCURS 0 WITH HEADER LINE,
      str(80).
PARAMETERS pname LIKE trdir-name OBLIGATORY.
START-OF-SELECTION.
  CHECK NOT pname IS INITIAL.
  READ REPORT pname INTO itab.
  CHECK sy-subrc = 0.
  EDITOR-CALL FOR itab BACKUP INTO itab.
  IF sy-subrc = 0.
    IF NOT itab[] IS INITIAL.
      INSERT REPORT pname FROM itab.
      IF sy-subrc = 0.
        CONCATENATE 'Program' pname 'is saved succesful!'
        INTO str SEPARATED BY space.
        WRITE: / str.
      ENDIF.
    ENDIF.
  ENDIF.
END-OF-SELECTION.