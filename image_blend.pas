{   Program BLEND <in1 fnam> <in1 weight> <in2 fnam> <in2 weight> <out fnam>
*    [ options ]
*
*   Blend the IN1 and IN2 images together to produce the OUT image.  The output image
*   will be computed by the formula:
*
*   OUT = IN1*WEIGHT1 + IN2*WEIGHT2 + ADD
*
*   The output image values will be clipped to the limits of the intensity range.
*   The two input files must be of the same size.
*
*   The command line options are:
*
*   -ADD <offset value>
*     Add a fixed amount to the output image.  The 0.0 to 1.0 range of OFFSET_VALUE
*     corresponds to the full intensity range.
}
program blend;
%include 'sys.ins.pas';
%include 'util.ins.pas';
%include 'string.ins.pas';
%include 'file.ins.pas';
%include 'img.ins.pas';

const
  max_image_width = 8192;              {max pixel image width this program can handle}
  max_x = max_image_width-1;           {max X pixel coordinate in scan line}
  max_msg_parms = 2;                   {max parameters we can pass to messages}

var
  in1_fnam,                            {first input file name}
  in2_fnam,                            {second input file name}
  out_fnam:                            {output file name}
    %include '(cog)lib/string_treename.ins.pas';
  in1_img: img_conn_t;                 {first input file handle}
  in2_img: img_conn_t;                 {second input file handle}
  out_img: img_conn_t;                 {handle to output image file}
  out_fmt:                             {format string for output image}
    %include '(cog)lib/string80.ins.pas';
  in1_scan,                            {first input file scan line}
  in2_scan,                            {second input file scan line}
  out_scan:                            {output file scan line}
    array[0..max_x] of img_pixel1_t;
  in1_wat: real;                       {weight factor for first input file}
  in2_wat: real;                       {weight factor for second input file}
  x: sys_int_machine_t;                {X pixel coordinate}
  val: real;                           {computed color value}
  i: sys_int_machine_t;                {loop counter}
  parm,                                {command line option parameter}
  opt:                                 {command line option}
    %include '(cog)lib/string80.ins.pas';
  pick: sys_int_machine_t;             {com line option number picked from list}
  add: real;                           {amount to add to output image in 0-255 scale}
  stat: sys_err_t;                     {completion status code}
  msg_parm:                            {parameter references for messages}
    array[1..max_msg_parms] of sys_parm_msg_t;

label
  next_opt, done_opts;

begin
  string_cmline_init;                  {init for processing command line}

  string_cmline_token (in1_fnam, stat); {get first input file name}
  string_cmline_req_check (stat);

  string_cmline_token_fpm (in1_wat, stat); {get weighting factor for first input image}
  string_cmline_req_check (stat);

  string_cmline_token (in2_fnam, stat); {get second input file name}
  string_cmline_req_check (stat);

  string_cmline_token_fpm (in2_wat, stat); {get weighting factor for second input image}
  string_cmline_req_check (stat);

  string_cmline_token (out_fnam, stat); {get output image file name}
  string_cmline_req_check (stat);

  img_open_read_img (in1_fnam, in1_img, stat); {open first input image for read}
  sys_msg_parm_vstr (msg_parm[1], in1_fnam);
  sys_error_abort (stat, 'img', 'open_read', msg_parm, 1);

  img_open_read_img (in2_fnam, in2_img, stat); {open second input image for read}
  sys_msg_parm_vstr (msg_parm[1], in2_fnam);
  sys_error_abort (stat, 'img', 'open_read', msg_parm, 1);

  if in1_img.x_size > max_image_width then begin {input image too wide ?}
    sys_message_bomb ('img', 'image_too_wide', nil, 0);
    end;

  if (in2_img.x_size <> in1_img.x_size )
      or (in2_img.y_size <> in1_img.y_size)
      then begin                       {input file sizes don't match ?}
    sys_message_bomb ('img', 'image_blend_different_sizes', nil, 0);
    end;
{
*   Set defaults before processing command line options.
}
  add := 0.0;                          {init to no offset added to output image}
{
*   Process the command line options.  Come back here each new command line
*   option.
}
next_opt:
  string_cmline_token (opt, stat);     {get next command line option name}
  if string_eos(stat) then goto done_opts; {exhausted command line ?}
  sys_error_abort (stat, 'string', 'cmline_opt_err', nil, 0);
  string_upcase (opt);                 {make upper case for matching list}
  string_tkpick80 (                    {pick option name from list}
    opt,                               {option name}
    '-ADD',                            {valid option names}
    pick);                             {number of picked option}
  case pick of                         {do routine for specific option}
{
*   -ADD offset
}
1: begin
  string_cmline_token_fpm (add, stat);
  add := add * 256.0;                  {convert to 8 bit pixel space}
  end;
{
*   Unrecognized command line option.
}
  otherwise
    string_cmline_opt_bad;             {complain about bad OPT and bomb}
    end;                               {end of command line option case statement}

  string_cmline_parm_check (stat, opt); {check for parameter error}
  goto next_opt;                       {back for next command line option}
done_opts:                             {done with all the command line options}
{
*   Open the output image file.
}
  string_list_pos_last (in1_img.comm); {move to last comment line}
  string_list_line_add (in1_img.comm); {create comment line for date}
  sys_date_time1 (in1_img.comm.str_p^); {init our comment line with date/time string}
  string_appends (in1_img.comm.str_p^,
    '  Blended with another image, using IMAGE_BLEND.');

  string_list_line_add (in1_img.comm); {create comment line for formula}
  string_appends (in1_img.comm.str_p^, '  New image =');
  string_append1 (in1_img.comm.str_p^, ' ');
  string_f_fp_fixed (parm, in1_wat, 3);
  string_append (in1_img.comm.str_p^, parm);
  string_appends (in1_img.comm.str_p^, '(this image) +');
  string_append1 (in1_img.comm.str_p^, ' ');
  string_f_fp_fixed (parm, in2_wat, 3);
  string_append (in1_img.comm.str_p^, parm);
  string_appends (in1_img.comm.str_p^, '(other image)');
  if add < 0.0
    then string_appendn (in1_img.comm.str_p^, ' - ', 3)
    else string_appendn (in1_img.comm.str_p^, ' + ', 3);
  string_f_fp_fixed (parm, abs(add/256.0), 3);
  string_append (in1_img.comm.str_p^, parm);

  string_list_line_add (in1_img.comm);
  string_appends (in1_img.comm.str_p^, '  This image came from file:');

  string_list_line_add (in1_img.comm);
  string_appendn (in1_img.comm.str_p^, '    ', 4);
  string_append (in1_img.comm.str_p^, in1_img.tnam);

  string_list_line_add (in1_img.comm);
  string_appends (in1_img.comm.str_p^, '  Other image came from file:');

  string_list_line_add (in1_img.comm);
  string_appendn (in1_img.comm.str_p^, '    ', 4);
  string_append (in1_img.comm.str_p^, in2_img.tnam);

  string_appendn (out_fmt, 'ALPHA ', 6); {make output file format request string}
  string_f_int (parm, max(in1_img.bits_alpha, in2_img.bits_alpha));
  string_append (out_fmt, parm);
  string_appendn (out_fmt, ' RED ', 5);
  string_f_int (parm, max(in1_img.bits_red, in2_img.bits_red));
  string_append (out_fmt, parm);
  string_appendn (out_fmt, ' GREEN ', 7);
  string_f_int (parm, max(in1_img.bits_grn, in2_img.bits_grn));
  string_append (out_fmt, parm);
  string_appendn (out_fmt, ' BLUE ', 6);
  string_f_int (parm, max(in1_img.bits_blu, in2_img.bits_blu));
  string_append (out_fmt, parm);

  img_open_write_img (                 {open output image for write}
    out_fnam,                          {output file name}
    in1_img.x_size / in1_img.y_size,   {aspect ratio}
    in1_img.x_size, in1_img.y_size,    {size of image in pixels}
    '',                                {select no explicit file type}
    out_fmt,                           {output file special requests}
    in1_img.comm,                      {comments list}
    out_img,                           {new image connection handle}
    stat);
  sys_msg_parm_vstr (msg_parm[1], out_fnam);
  sys_error_abort (stat, 'img', 'open_write', msg_parm, 1);
{
*   The output image is now open.
}
  for i := 1 to in1_img.y_size do begin {once for each scan line}
    img_read_scan1 (in1_img, in1_scan, stat); {read this input scan line}
    sys_error_abort (stat, 'img', 'read_scan_line', nil, 0);
    img_read_scan1 (in2_img, in2_scan, stat); {read this input scan line}
    sys_error_abort (stat, 'img', 'read_scan_line', nil, 0);
    for x := 0 to in1_img.x_size-1 do begin {once for each pixel in scan lines}

      val :=                           {make blended intensity value}
        ord(in1_scan[x].alpha)*in1_wat +
        ord(in2_scan[x].alpha)*in2_wat + add;
      val := min(255.0, max(0.0, val)); {clip to allowable range}
      out_scan[x].alpha := trunc(val); {convert to integer and store in out scan}

      val :=                           {make blended intensity value}
        ord(in1_scan[x].red)*in1_wat +
        ord(in2_scan[x].red)*in2_wat + add;
      val := min(255.0, max(0.0, val)); {clip to allowable range}
      out_scan[x].red := trunc(val);   {convert to integer and store in out scan}

      val :=                           {make blended intensity value}
        ord(in1_scan[x].grn)*in1_wat +
        ord(in2_scan[x].grn)*in2_wat + add;
      val := min(255.0, max(0.0, val)); {clip to allowable range}
      out_scan[x].grn := trunc(val);   {convert to integer and store in out scan}

      val :=                           {make blended intensity value}
        ord(in1_scan[x].blu)*in1_wat +
        ord(in2_scan[x].blu)*in2_wat + add;
      val := min(255.0, max(0.0, val)); {clip to allowable range}
      out_scan[x].blu := trunc(val);   {convert to integer and store in out scan}

      end;                             {back and do next pixel in this scan line}
    img_write_scan1 (out_img, out_scan, stat); {write this scan line to output file}
    sys_error_abort (stat, 'img', 'write_scan_line', nil, 0);
    end;                               {back and do next scan line in images}

  img_close (in1_img, stat);           {close first input file}
  sys_error_print (stat, 'img', 'close', nil, 0);
  img_close (in2_img, stat);           {close second input file}
  sys_error_print (stat, 'img', 'close', nil, 0);
  img_close (out_img, stat);           {close output file}
  sys_error_print (stat, 'img', 'close', nil, 0);
  end.
