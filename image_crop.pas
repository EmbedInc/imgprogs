{   Program IMAGE_CROP [options]
*
*   Copy a rectangle of the input image to the output image.
*
*   <input file name>
*
*   -IN <input file name>
*
*   <output file name>
*
*   -OUT <output file name>
*
*   -UL x y
*
*   -SIZE nx ny
*
*   -COM <comment string>
*
*   -FTYPE <image output type>
*
*   -FORM <output format string>
}
program image_resize;
%include 'sys.ins.pas';
%include 'util.ins.pas';
%include 'string.ins.pas';
%include 'file.ins.pas';
%include 'img.ins.pas';

const
  max_msg_args = 2;                    {max arguments we can pass to a message}

var
  fnam_in, fnam_out:                   {input and output file names}
    %include '(cog)lib/string_treename.ins.pas';
  form: string_var8192_t;              {user output image format string}
  ftype: string_var32_t;               {output image file type name}
  xlft, xrit, ybot, ytop: sys_int_machine_t; {output rectangle edge pixels in input image}
  nox, noy: sys_int_machine_t;         {output image size}
  iname_set: boolean;                  {TRUE if the input file name already set}
  oname_set: boolean;                  {TRUE if the output file name already set}
  img_in, img_out: img_conn_t;         {connection handles to in and out images}
  inasp: real;                         {full input image aspect ratio}
  comm: string_list_t;                 {image file comment lines}
  ucomm: string_list_t;                {explicit user comments}
  sz: sys_int_adr_t;                   {memory size}
  paspect: real;                       {pixel aspect ratio}
  ix, iy: sys_int_machine_t;           {current input pixel coordinate}
  ox, oy: sys_int_machine_t;           {current output pixel coordinate}
  iscan_p: img_scan2_arg_p_t;          {pointer to input scan line buffer}
  oscan_p: img_scan2_arg_p_t;          {pointer to output scan line buffer}
  backg: img_pixel2_t;                 {background color to use when outside image}
  r: real;                             {scratch floating point}
  size_set: boolean;                   {output image size explicitly set}
  ul_set: boolean;                     {upper left within source image explicitly set}
  inasp_set: boolean;                  {input image aspect ratio explicitly set}

  opt:                                 {upcased command line option}
    %include '(cog)lib/string_treename.ins.pas';
  parm:                                {command line option parameter}
    %include '(cog)lib/string_treename.ins.pas';
  pick: sys_int_machine_t;             {number of token picked from list}
  msg_parm:                            {references arguments passed to a message}
    array[1..max_msg_args] of sys_parm_msg_t;
  stat: sys_err_t;                     {completion status code}

label
  next_opt, err_parm, parm_bad, done_opts, have_oscan, leave;
{
******************************************************************
*
*   Start of main routine.
}
begin
  form.max := sizeof(form.str);        {init local var strings}
  ftype.max := sizeof(ftype.str);

  string_cmline_init;                  {init for reading the command line}
{
*   Initialize our state before reading the command line options.
}
  iname_set := false;                  {no input file name specified}
  oname_set := false;                  {no output file name specified}
  inasp_set := false;                  {no input image aspect ratio override}
  xlft := 0;                           {init output rectangle in input image}
  nox := 1000000000;
  ytop := 0;
  noy := 1000000000;
  string_list_init (ucomm, util_top_mem_context); {init explicit user comments list}
  ucomm.deallocable := false;
  form.len := 0;                       {init to no output format string supplied}
  ftype.len := 0;                      {init to use default output file type}
  backg.red := 0;                      {init background to black}
  backg.grn := 0;
  backg.blu := 0;
  backg.alpha := 65535;
  size_set := false;
  ul_set := false;
{
*   Back here each new command line option.
}
next_opt:
  string_cmline_token (opt, stat);     {get next command line option name}
  if string_eos(stat) then goto done_opts; {exhausted command line ?}
  sys_error_abort (stat, 'string', 'cmline_opt_err', nil, 0);
  if (opt.len >= 1) and (opt.str[1] <> '-') then begin {implicit pathname token ?}
    if not iname_set then begin        {input file name not set yet ?}
      string_copy (opt, fnam_in);      {set input file name}
      iname_set := true;               {input file name is now set}
      goto next_opt;
      end;
    if not oname_set then begin        {output file name not set yet ?}
      string_copy (opt, fnam_out);     {set output file name}
      oname_set := true;               {output file name is now set}
      goto next_opt;
      end;
    sys_msg_parm_vstr (msg_parm[1], opt);
    sys_message_bomb ('string', 'cmline_opt_conflict', msg_parm, 1);
    end;
  string_upcase (opt);                 {make upper case for matching list}
  string_tkpick80 (opt,                {pick command line option name from list}
    '-IN -OUT -SIZE -UL -COM -FTYPE -FORM -BACKG -INASP',
    pick);                             {number of keyword picked from list}
  case pick of                         {do routine for specific option}
{
*   -IN filename
}
1: begin
  if iname_set then begin              {input file name already set ?}
    sys_msg_parm_vstr (msg_parm[1], opt);
    sys_message_bomb ('string', 'cmline_opt_conflict', msg_parm, 1);
    end;
  string_cmline_token (fnam_in, stat);
  iname_set := true;
  end;
{
*   -OUT filename
}
2: begin
  if oname_set then begin              {output file name already set ?}
    sys_msg_parm_vstr (msg_parm[1], opt);
    sys_message_bomb ('string', 'cmline_opt_conflict', msg_parm, 1);
    end;
  string_cmline_token (fnam_out, stat);
  oname_set := true;
  end;
{
*   -SIZE dx dy
}
3: begin
  string_cmline_token_int (nox, stat);
  if sys_error(stat) then goto err_parm;
  if nox < 1 then goto parm_bad;
  string_cmline_token_int (noy, stat);
  if sys_error(stat) then goto err_parm;
  if noy < 1 then goto parm_bad;
  size_set := true;
  end;
{
*   -UL x y
}
4: begin
  string_cmline_token_int (xlft, stat);
  if sys_error(stat) then goto err_parm;
  string_cmline_token_int (ytop, stat);
  ul_set := true;
  end;
{
*   -COM <comment>
}
5: begin
  string_cmline_token (parm, stat);
  if sys_error(stat) then goto err_parm;
  ucomm.size := parm.len;
  string_list_line_add (ucomm);        {create new user comments line}
  string_copy (parm, ucomm.str_p^);    {save user comment line}
  end;
{
*   -FTYPE filetype
}
6: begin
  string_cmline_token (ftype, stat);
  end;
{
*   -FORM <output format string>
}
7: begin
  string_cmline_token (parm, stat);
  if sys_error(stat) then goto err_parm;
  if form.len > 0 then begin           {there is a previous format string ?}
    string_append1 (form, ' ');        {add separator before new format string}
    end;
  string_append (form, parm);
  end;
{
*   -BACKG red grn blu
}
8: begin
  string_cmline_token_fpm (r, stat);
  if sys_error(stat) then goto err_parm;
  backg.red := round(max(0.0, min(65535.0, r*65536.0)));
  string_cmline_token_fpm (r, stat);
  if sys_error(stat) then goto err_parm;
  backg.grn := round(max(0.0, min(65535.0, r*65536.0)));
  string_cmline_token_fpm (r, stat);
  if sys_error(stat) then goto err_parm;
  backg.blu := round(max(0.0, min(65535.0, r*65536.0)));
  end;
{
*   -INASP ratio
}
9: begin
  string_cmline_token_fpm (inasp, stat);
  if sys_error(stat) then goto err_parm;
  inasp_set := true;
  end;
{
*   Unrecognized command line option.
}
otherwise
    string_cmline_opt_bad;             {unrecognized command line option}
    end;                               {end of command line option case statement}

err_parm:                              {jump here on error with parameter}
  string_cmline_parm_check (stat, opt); {check for bad command line option parameter}
  goto next_opt;                       {back for next command line option}

parm_bad:                              {jump here on got illegal parameter}
  string_cmline_reuse;                 {re-read last command line token next time}
  string_cmline_token (parm, stat);    {re-read the token for the bad parameter}
  sys_msg_parm_vstr (msg_parm[1], parm);
  sys_msg_parm_vstr (msg_parm[2], opt);
  sys_message_bomb ('string', 'cmline_parm_bad', msg_parm, 2);

done_opts:                             {done with all the command line options}
{
*   All done reading the command line.
*   Now open the input file and adjust to its size.
}
  if not iname_set then begin          {no input file name specified ?}
    sys_message_bomb ('img', 'input_fnam_missing', nil, 0);
    end;

  img_open_read_img (                  {try to open the input image}
    fnam_in,                           {input image file name}
    img_in,                            {returned connection handle}
    stat);
  sys_msg_parm_vstr (msg_parm[1], fnam_in);
  sys_error_abort (stat, 'img', 'open_read', msg_parm, 1);

  if not inasp_set then begin          {input image aspect ratio not overridden ?}
    inasp := img_in.aspect;            {use aspect ratio from input image file}
    end;

  if not size_set then begin           {crop region size not set ?}
    nox := img_in.x_size;              {init to size of input image}
    noy := img_in.y_size;
    end;
  if not ul_set then begin             {position of crop region not set ?}
    xlft := (img_in.x_size - nox) div 2; {center the crop region in the input image}
    ytop := (img_in.y_size - noy) div 2;
    end;
  xrit := xlft + nox - 1;              {make implied right/bottom edge limits}
  ybot := ytop + noy - 1;

  paspect := inasp * img_in.y_size / img_in.x_size; {make pixel aspect ratio}
  sz := sizeof(iscan_p^[0]) * img_in.x_size; {allocate input scan line}
  img_mem_alloc (img_in, sz, iscan_p);
{
*   Open the image output file.
}
  if not oname_set then begin          {use default output file name ?}
    string_copy (img_in.gnam, fnam_out); {use generic name of input image file}
    end;

  string_list_copy (                   {make local copy of input image comments}
    img_in.comm, comm, util_top_mem_context);
  string_list_pos_last (comm);         {go to last comment line in list}

  sys_date_time1 (opt);                {add our first comment line}
  string_appends (opt, '  Cropped from image_file'(0));
  comm.size := opt.len;
  string_list_line_add (comm);
  string_copy (opt, comm.str_p^);

  comm.size := img_in.tnam.len + 2;    {add our second comment line}
  string_list_line_add (comm);
  string_vstring (comm.str_p^, '  '(0), -1);
  string_append (comm.str_p^, img_in.tnam);

  opt.len := 0;                        {add our third comment line}
  string_appends (opt, '  on machine '(0));
  sys_node_name (parm);
  string_upcase (parm);
  string_append (opt, parm);
  string_appends (opt, ' by IMAGE_CROP -UL '(0));
  string_f_int (parm, xlft);
  string_append (opt, parm);
  string_appends (opt, ' '(0));
  string_f_int (parm, ytop);
  string_append (opt, parm);
  string_appends (opt, ' -SIZE '(0));
  string_f_int (parm, nox);
  string_append (opt, parm);
  string_appends (opt, ' '(0));
  string_f_int (parm, noy);
  string_append (opt, parm);
  if inasp_set then begin
    string_f_fp_free (parm, inasp, 5);
    string_appends (opt, ' -INASP '(0));
    string_append (opt, parm);
    end;
  comm.size := opt.len;
  string_list_line_add (comm);
  string_copy (opt, comm.str_p^);

  {  Add on user comment lines, if any.
  }
  string_list_pos_abs (ucomm, 1);      {go to first user comment line}
  while ucomm.str_p <> nil do begin    {once for each user comment line}
    comm.size := ucomm.str_p^.len + 2;
    string_list_line_add (comm);
    string_appendn (comm.str_p^, '  ', 2); {indent user comment line}
    string_append (comm.str_p^, ucomm.str_p^);
    string_list_pos_rel (ucomm, 1);    {advance to next user comment line}
    end;
  string_list_kill (ucomm);            {all done with user comment lines list}

  string_vstring (opt, 'RED '(0), -1); {make driver parameters string}
  string_f_int (parm, min(img_in.bits_red, 8));
  string_append (opt, parm);
  string_appends (opt, ' GREEN '(0));
  string_f_int (parm, min(img_in.bits_grn, 8));
  string_append (opt, parm);
  string_appends (opt, ' BLUE '(0));
  string_f_int (parm, min(img_in.bits_blu, 8));
  string_append (opt, parm);
  string_appends (opt, ' ALPHA '(0));
  string_f_int (parm, min(img_in.bits_alpha, 8));
  string_append (opt, parm);
  if form.len > 0 then begin           {add user format string, if any}
    string_append1 (opt, ' ');
    string_append (opt, form);
    end;

  string_terminate_null (ftype);       {make sure string body is null terminated}

  img_open_write_img (                 {open output image}
    fnam_out,                          {image file name}
    paspect * nox / noy,               {whole image aspect ratio}
    nox, noy,                          {image size in pixels}
    ftype.str,                         {driver name or will default from FNAM_OUT}
    opt,                               {driver format string}
    comm,                              {comment lines descriptor}
    img_out,                           {returned connection handle to new image}
    stat);
  sys_msg_parm_vstr (msg_parm[1], fnam_out);
  sys_error_abort (stat, 'img', 'open_write', msg_parm, 1);
  sz := sizeof(oscan_p^[0]) * img_out.x_size; {allocate input scan line}
  img_mem_alloc (img_out, sz, oscan_p);
{
*   Loop thru the input image and write the output image as appropriate.
}
  for oy := 0 to img_out.y_size-1 do begin {down the output image scan lines}
    iy := ytop + oy;                   {make corresponding input image scan line}
    if (iy < 0) or (iy >= img_in.y_size) then begin {above or below input image ?}
      for ox := 0 to img_out.x_size-1 do begin {accross the output image scan line}
        oscan_p^[ox] := backg;         {set whole line to background color}
        end;
      goto have_oscan;                 {OSCAN all set for this output scan line}
      end;
    while (img_in.next_y - 1) < iy do begin {read input lines until get to curr line}
      img_read_scan2 (img_in, iscan_p^, stat); {read next input image scan line}
      sys_error_abort (stat, 'img', 'read_scan_line', nil, 0);
      end;
    for ox := 0 to img_out.x_size-1 do begin {once for each output pixel this scan line}
      ix := xlft + ox;                 {make input X coor of this pixel}
      if (ix >= 0) and (ix < img_in.x_size)
        then begin                     {input pixel exists at this location}
          oscan_p^[ox] := iscan_p^[ix]; {copy output pixel from input image}
          end
        else begin                     {left or right of input image}
          oscan_p^[ox] := backg;       {set to background color}
          end
        ;
      end;                             {back to set next output pixel}
have_oscan:                            {OSCAN_P^ all set for this output scan line}
    img_write_scan2 (                  {write cropped section of this scan line to output}
      img_out,                         {connection to output image}
      oscan_p^,                        {scan line to write}
      stat);
    sys_error_abort (stat, 'img', 'write_scan_line', nil, 0);
    end;                               {back for next output image scan line}
{
*   All done writing the output image.  Clean up and leave.
}
leave:                                 {common exit point}
  img_close (img_out, stat);           {close output image}
  img_close (img_in, stat);            {close input image}
  end.
