{   Program IMAGE_MASK
}
program image_mask;
%include 'sys.ins.pas';
%include 'util.ins.pas';
%include 'string.ins.pas';
%include 'file.ins.pas';
%include 'img.ins.pas';

const
  max_msg_args = 2;                    {max arguments we can pass to a message}

var
  fnam_in:                             {input file name}
    %include '(cog)lib/string_treename.ins.pas';
  fnam_out:                            {output file name}
    %include '(cog)lib/string_treename.ins.pas';
  iname_set: boolean;                  {TRUE if the input file name already set}
  oname_set: boolean;                  {TRUE if the output file name already set}
  thred, thgrn, thblu: real;           {foreground thresholds, 0-1 scale}
  thired, thigrn, thiblu: sys_int_machine_t; {foreground thresholds, 0-255 scale}
  inv: boolean;                        {thresholds inverted}
  blk: boolean;                        {write foreground as black, not white}
  conn_in: img_conn_t;                 {connection to input image}
  conn_out: img_conn_t;                {connection to output image}
  fmt:                                 {output image format string}
    %include '(cog)lib/string256.ins.pas';
  comm: string_list_t;                 {output image list of comments}
  scan_p: img_scan1_arg_p_t;           {pointer to one image scan line}
  x, y: sys_int_machine_t;             {pixel coordinate}
  forg: boolean;                       {current pixel is foreground}

  opt:                                 {upcased command line option}
    %include '(cog)lib/string_treename.ins.pas';
  parm:                                {command line option parameter}
    %include '(cog)lib/string_treename.ins.pas';
  pick: sys_int_machine_t;             {number of token picked from list}
  msg_parm:                            {references arguments passed to a message}
    array[1..max_msg_args] of sys_parm_msg_t;
  stat: sys_err_t;                     {completion status code}

label
  next_opt, err_parm, parm_bad, done_opts;

begin
{
*   Initialize before reading the command line.
}
  string_cmline_init;                  {init for reading the command line}
  iname_set := false;                  {no input file name specified}
  oname_set := false;                  {no output file name specified}
  thred := 0.5;                        {init foreground color threshold}
  thgrn := 0.5;
  thblu := 0.5;
  inv := false;                        {init to foreground above threshold}
  blk := false;                        {init to write foreground as white}
{
*   Back here each new command line option.
}
next_opt:
  string_cmline_token (opt, stat);     {get next command line option name}
  if string_eos(stat) then goto done_opts; {exhausted command line ?}
  sys_error_abort (stat, 'string', 'cmline_opt_err', nil, 0);
  if (opt.len >= 1) and (opt.str[1] <> '-') then begin {implicit pathname token ?}
    if not iname_set then begin        {input file name not set yet ?}
      string_treename(opt, fnam_in);   {set input file name}
      iname_set := true;               {input file name is now set}
      goto next_opt;
      end;
    if not oname_set then begin        {output file name not set yet ?}
      string_treename (opt, fnam_out); {set output file name}
      oname_set := true;               {output file name is now set}
      goto next_opt;
      end;
    sys_msg_parm_vstr (msg_parm[1], opt);
    sys_message_bomb ('string', 'cmline_opt_conflict', msg_parm, 1);
    end;
  string_upcase (opt);                 {make upper case for matching list}
  string_tkpick80 (opt,                {pick command line option name from list}
    '-IN -OUT -TH -INV -BLK',
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
  string_cmline_token (opt, stat);
  string_treename (opt, fnam_in);
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
  string_cmline_token (opt, stat);
  string_treename (opt, fnam_out);
  oname_set := true;
  end;
{
*   -TH red grn blu
}
3: begin
  string_cmline_token_fpm (thred, stat);
  if sys_error(stat) then goto parm_bad;
  string_cmline_token_fpm (thgrn, stat);
  if sys_error(stat) then goto parm_bad;
  string_cmline_token_fpm (thblu, stat);
  if sys_error(stat) then goto parm_bad;
  end;
{
*   -INV
}
4: begin
  inv := true;
  end;
{
*   -BLK
}
5: begin
  blk := true;
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
  if not iname_set then begin
    sys_message_bomb ('string', 'cmline_input_fnam_missing', nil, 0);
    end;
{
*  Open the input image.
}
  img_open_read_img (                  {open the input image}
    fnam_in,                           {image file name}
    conn_in,                           {returned connection to the image}
    stat);
  sys_error_abort (stat, '', '', nil, 0);

  writeln ('Reading "', conn_in.tnam.str:conn_in.tnam.len, '"');
  writeln ('  ', conn_in.x_size, ' x ', conn_in.y_size, ' pixels');
{
*   Open the output image.
}
  if not oname_set then begin          {use default output file name ?}
    string_copy (conn_in.gnam, fnam_out); {init with generic input file name}
    if blk
      then begin
        string_appends (fnam_out, '_blk'(0));
        end
      else begin
        string_appends (fnam_out, '_wht'(0));
        end
      ;
    string_appends (fnam_out, '.tif'(0));
    end;

  string_list_init (comm, util_top_mem_context); {init comment lines list}
  string_vstring (fmt, 'RED 8 GREEN 8 BLUE 8 ALPHA 8'(0), -1);

  img_open_write_img (                 {open the output image}
    fnam_out,                          {image file name}
    conn_in.aspect,                    {whole image width/height aspect ratio}
    conn_in.x_size, conn_in.y_size,    {image size in pixels}
    '',                                {explicit image file type string}
    fmt,                               {format string}
    comm,                              {comment lines}
    conn_out,                          {returned connection to the image}
    stat);
  sys_error_abort (stat, '', '', nil, 0);
  writeln ('Writing "', conn_out.tnam.str:conn_out.tnam.len, '"');

  thred := max(0.0, min(1.0, thred));  {clip thresholds to valid range}
  thgrn := max(0.0, min(1.0, thgrn));
  thblu := max(0.0, min(1.0, thblu));

  thired := min(255, round(thred * 256.0)); {make 0-255 integer thresholds}
  thigrn := min(255, round(thgrn * 256.0));
  thiblu := min(255, round(thblu * 256.0));
{
*   Copy from the input image to the output image, while applying our
*   transformation.
}
  img_mem_alloc (                      {allocate memory for one scan line}
    conn_in,                           {image to associate memory with}
    sizeof(scan_p^[0]) * conn_in.x_size, {size for one scan line of pixels}
    scan_p);                           {returned pointer to the new memory}

  for y := 0 to conn_in.y_size-1 do begin {down the scan lines}
    img_read_scan1 (conn_in, scan_p^, stat); {read this scan line}
    sys_error_abort (stat, '', '', nil, 0);

    for x := 0 to conn_in.x_size-1 do begin {across this scan line}
      {
      *   Threshold the input.  Set FORG to indicate whether this is a
      *   foreground or background pixel.
      }
      if inv
        then begin                     {foreground is at or below threshold}
          forg :=                      {foreground color ?}
            (scan_p^[x].red <= thired) or
            (scan_p^[x].grn <= thigrn) or
            (scan_p^[x].blu <= thiblu);
          end
        else begin                     {foreground is at or above threshold}
          forg :=                      {foreground color ?}
            (scan_p^[x].red >= thired) or
            (scan_p^[x].grn >= thigrn) or
            (scan_p^[x].blu >= thiblu);
          end
        ;
      {
      *   Set the pixel to the fixed color/opacity depending on whether it is
      *   foreground or background.
      }
      if forg
        then begin                     {this is a foreground pixel}
          if blk
            then begin                 {write foreground a opaque black}
              scan_p^[x].red := 0;
              scan_p^[x].grn := 0;
              scan_p^[x].blu := 0;
              scan_p^[x].alpha := 255;
              end
            else begin                 {write foreground as opaque white}
              scan_p^[x].red := 255;
              scan_p^[x].grn := 255;
              scan_p^[x].blu := 255;
              scan_p^[x].alpha := 255;
              end
            ;
          end
        else begin                     {this is a background pixel}
          scan_p^[x].red := 0;         {make fully transparent}
          scan_p^[x].grn := 0;
          scan_p^[x].blu := 0;
          scan_p^[x].alpha := 0;
          end
        ;
      end;                             {back to transform next pixel this scan line}

    img_write_scan1 (conn_out, scan_p^, stat); {write this scan line}
    sys_error_abort (stat, '', '', nil, 0);
    end;                               {back for next scan line in this image}
{
*   Clean up and leave.
}
  img_close (conn_out, stat);
  sys_error_abort (stat, '', '', nil, 0);
  img_close (conn_in, stat);
  sys_error_abort (stat, '', '', nil, 0);
  end.
