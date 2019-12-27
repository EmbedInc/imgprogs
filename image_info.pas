{   Program IMAGE_INFO <image file name>
*
*   Print information about an image.
}
program image_info;
%include 'sys.ins.pas';
%include 'util.ins.pas';
%include 'string.ins.pas';
%include 'file.ins.pas';
%include 'img.ins.pas';

const
  max_msg_parms = 8;                   {max parms we can pass to a message}

var
  in_fnam:                             {input file name}
    %include '(cog)lib/string_treename.ins.pas';
  img: img_conn_t;                     {handle to image file connection}
  stat: sys_err_t;
  msg_parms:                           {pointers to parameters passed to messages}
    array[1..max_msg_parms] of sys_parm_msg_t;
  ftypeu:                              {upper case image file type}
    %include '(cog)lib/string_leafname.ins.pas';
  i: sys_int_machine_t;                {loop counter}
  buf:                                 {one line output buffer}
    %include '(cog)lib/string_treename.ins.pas';

begin
  string_cmline_init;                  {init for command line reading}
  string_cmline_token (in_fnam, stat); {get image file name from command line}
  if string_eos(stat) then begin
    sys_message ('string', 'cmline_arg1_missing');
    sys_bomb;
    end;
  string_cmline_end_abort;             {make sure we got to end of command line}

  img_open_read_img (in_fnam, img, stat); {try to open image file for read}
  sys_msg_parm_vstr (msg_parms[1], in_fnam);
  sys_error_abort (stat, 'img', 'open_read', msg_parms, 1);
{
*   The image file opened successfully.
}
  sys_msg_parm_vstr (msg_parms[1], img.tnam); {print full image file name}
  sys_message_parms ('img', 'image_info_fnam', msg_parms, 1);

  sys_msg_parm_int (msg_parms[1], img.x_size); {print other data about this image}
  sys_msg_parm_int (msg_parms[2], img.y_size);
  sys_msg_parm_real (msg_parms[3], img.aspect);
  sys_msg_parm_int (msg_parms[4], img.bits_alpha);
  sys_msg_parm_int (msg_parms[5], img.bits_red);
  sys_msg_parm_int (msg_parms[6], img.bits_grn);
  sys_msg_parm_int (msg_parms[7], img.bits_blu);
  string_copy (img.file_type, ftypeu);
  string_upcase (ftypeu);
  sys_msg_parm_vstr (msg_parms[8], ftypeu);
  sys_message_parms ('img', 'image_info_data', msg_parms, 8);
{
*   Print the comments from the image file.
}
  if img.comm.n > 0 then begin         {there is at least one comment line ?}
    writeln;                           {leave a blank line before first comment}
    for i := 1 to img.comm.n do begin  {once for each comment line}
      string_list_pos_rel (img.comm, 1); {go to this comment line}
      buf.len := 0;                    {init output line}
      string_appendn (buf, '  ', 2);   {indent comments}
      string_appendn (buf, img.comm.str_p^.str, img.comm.str_p^.len);
      writeln (buf.str:buf.len);
      end;                             {back for next comment line}
    end;                               {done handling comment lines exist}

  img_close (img, stat);               {close image file}
  sys_msg_parm_vstr (msg_parms[1], img.tnam);
  sys_error_abort (stat, 'img', 'close', msg_parms, 1);
  end.
