{   Program IMAGE_HIST <image file name>
}
program image_hist;
%include 'sys.ins.pas';
%include 'util.ins.pas';
%include 'string.ins.pas';
%include 'file.ins.pas';
%include 'img.ins.pas';

const
  max_msg_args = 4;                    {max arguments we can pass to a message}

type
  cnt_t = record                       {occurrence counters for one 0-255 value}
    red: sys_int_machine_t;            {number of pixels with this value}
    grn: sys_int_machine_t;
    blu: sys_int_machine_t;
    alpha: sys_int_machine_t;
    end;

var
  fnam_in,                             {input image file name}
  fnam_out:                            {histogram output file name}
    %include '(cog)lib/string_treename.ins.pas';
  buf:                                 {one line output buffer}
    %include '(cog)lib/string80.ins.pas';
  token:                               {token for string conversion}
    %include '(cog)lib/string32.ins.pas';
  img: img_conn_t;                     {connection handle to image}
  scan_p: img_scan1_arg_p_t;           {pointer to memory for one scan line}
  scan_size: sys_int_adr_t;            {size of scan line buffer}
  conn: file_conn_t;                   {connection handle to histogram output file}
  i: sys_int_machine_t;                {scratch integer and loop counter}
  r: real;                             {scratch floating point number}
  y: sys_int_machine_t;                {scan line loop counter}
  x: sys_int_machine_t;                {pixel X coordinate}
  red_acc, grn_acc, blu_acc, alpha_acc: {accumulators for finding average color}
    sys_int_conv32_t;
  ave_red, ave_grn, ave_blu, ave_alpha: {final average values}
    real;
  cnt:                                 {counters per color per value}
    array[0..255] of cnt_t;
  max_red, max_grn, max_blu, max_alpha: {max counter values for each component}
    sys_int_conv32_t;
  msg_parm:                            {references arguments passed to a message}
    array[1..max_msg_args] of sys_parm_msg_t;
  stat: sys_err_t;                     {completion status code}

begin
  string_cmline_init;                  {init command line}
  string_cmline_token (fnam_in, stat); {get input image file name}
  string_cmline_req_check (stat);

  img_open_read_img (fnam_in, img, stat); {open input image for read}
  sys_msg_parm_vstr (msg_parm[1], fnam_in);
  sys_error_abort (stat, 'img', 'open_read', msg_parm, 1);

  string_cmline_token (fnam_out, stat); {try for optional output file name}
  if string_eos(stat) then begin       {output file name defaulted ?}
    string_copy (img.gnam, fnam_out);  {init to input image file generic leafname}
    string_appends (fnam_out, '_hist');
    end;
  sys_error_abort (stat, 'string', 'cmline_opt_err', nil, 0);

  string_cmline_end_abort;             {no more command line parms allowed}

  file_open_write_text (fnam_out, '', conn, stat); {open histogram output file}
  sys_msg_parm_vstr (msg_parm[1], fnam_out);
  sys_error_abort (stat, 'file', 'open_output_write_text', msg_parm, 1);

  for i := 0 to 255 do begin           {init all the counters to zero}
    cnt[i].red := 0;
    cnt[i].grn := 0;
    cnt[i].blu := 0;
    cnt[i].alpha := 0;
    end;

  red_acc := 0;                        {init accumulators for finding averages}
  grn_acc := 0;
  blu_acc := 0;
  alpha_acc := 0;

  scan_size := img.x_size * sizeof(scan_p^); {allocate memory for one scan line}
  img_mem_alloc (img, scan_size, scan_p);

  for y := 0 to img.y_size-1 do begin  {down the scan lines}
    img_read_scan1 (img, scan_p^, stat); {read in this scan line}
    sys_error_abort (stat, 'img', 'read_scan_line', nil, 0);
    for x := 0 to img.x_size-1 do begin {accross this scan line}
      cnt[scan_p^[x].red].red := cnt[scan_p^[x].red].red + 1; {count occurrences}
      cnt[scan_p^[x].grn].grn := cnt[scan_p^[x].grn].grn + 1;
      cnt[scan_p^[x].blu].blu := cnt[scan_p^[x].blu].blu + 1;
      cnt[scan_p^[x].alpha].alpha := cnt[scan_p^[x].alpha].alpha + 1;
      red_acc := red_acc + scan_p^[x].red; {accumulate component value totals}
      grn_acc := grn_acc + scan_p^[x].grn;
      blu_acc := blu_acc + scan_p^[x].blu;
      alpha_acc := alpha_acc + scan_p^[x].alpha;
      end;
    end;

  img_close (img, stat);               {close input image file}
  sys_msg_parm_vstr (msg_parm[1], img.tnam);
  sys_error_abort (stat, 'img', 'close', msg_parm, 1);

  max_red := 0;                        {max occurrences for each component}
  max_grn := 0;
  max_blu := 0;
  max_alpha := 0;

  for i := 0 to 255 do begin           {loop to find max counter value}
    max_red := max(max_red, cnt[i].red);
    max_grn := max(max_grn, cnt[i].grn);
    max_blu := max(max_blu, cnt[i].blu);
    max_alpha := max(max_alpha, cnt[i].alpha);
    end;

  for i := 0 to 255 do begin           {loop to print the values}
    r := i / 255.0;                    {write intensity value for this bucket}
    string_f_fp_ftn (buf, r, 5, 3);

    if cnt[i].red = 0
      then begin                       {value is exactly zero}
        token.len := 0;
        string_appendn (token, '   0     ', 9);
        end
      else begin                       {value is not exactly zero}
        r := 100.0 * cnt[i].red / max_red;
        string_f_fp_ftn (token, r, 9, 4);
        end
      ;
    string_append (buf, token);

    if cnt[i].grn = 0
      then begin                       {value is exactly zero}
        token.len := 0;
        string_appendn (token, '   0     ', 9);
        end
      else begin                       {value is not exactly zero}
        r := 100.0 * cnt[i].grn / max_grn;
        string_f_fp_ftn (token, r, 9, 4);
        end
      ;
    string_append (buf, token);

    if cnt[i].blu = 0
      then begin                       {value is exactly zero}
        token.len := 0;
        string_appendn (token, '   0     ', 9);
        end
      else begin                       {value is not exactly zero}
        r := 100.0 * cnt[i].blu / max_blu;
        string_f_fp_ftn (token, r, 9, 4);
        end
      ;
    string_append (buf, token);

    if cnt[i].alpha = 0
      then begin                       {value is exactly zero}
        token.len := 0;
        string_appendn (token, '   0     ', 9);
        end
      else begin                       {value is not exactly zero}
        r := 100.0 * cnt[i].alpha / max_alpha;
        string_f_fp_ftn (token, r, 9, 4);
        end
      ;
    string_append (buf, token);

    file_write_text (buf, conn, stat); {write this line to histogram file}
    sys_error_abort (stat, 'file', 'write_output_text', nil, 0);
    end;

  file_close (conn);                   {close histogram output file}
{
*   Print pixel component averages.
}
  r := 1.0 / ( 255.0 * img.x_size * img.y_size); {mult factor to make final averages}

  ave_red := red_acc * r;              {compute final averages}
  ave_grn := grn_acc * r;
  ave_blu := blu_acc * r;
  ave_alpha := alpha_acc * r;

  sys_msg_parm_real (msg_parm[1], ave_red);
  sys_msg_parm_real (msg_parm[2], ave_grn);
  sys_msg_parm_real (msg_parm[3], ave_blu);
  sys_msg_parm_real (msg_parm[4], ave_alpha);
  sys_message_parms ('img', 'image_hist_averages', msg_parm, 4);
  end.
