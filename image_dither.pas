{   Program IMAGE_DITHER <in file name> [<options>]
*
*   Apply a dither pattern to the input image to create an output image.
*   Valid command line options are:
*
*   -OUT filename
*
*     Specify the output image file name explicitly.  The default is a file in the
*     working directory with the same generic name as the input file name.
*
*   -BITS r g b
*
*     Indicate how many bits per color the final output image should have.
*     Note that this is a subset of the more general -LEVELS command line option.
*     The -BITS command line option is mutually exclusive with the -LEVELS
*     command line option.  The default levels per color can not be expressed
*     with the -BITS option.
*
*   -LEVELS r g b
*
*     Indicate how many actual levels per color the final output image
*     should have.  This is not the apparent levels per color achieved by
*     dithering, but the maximum actual levels for all output pixels for
*     each color.  The -LEVELS command line option is mutually exclusive
*     with the -BITS command line option.  The default is -LEVELS 6 7 6,
*     which uses a total of 252 possible red/green/blue combinations, and
*     therefore fits into 8 bits/pixel.
*
*   -PATT name
*
*     Selects a pre-defined dither pattern.  The predefined dither patterns
*     come from regular dither pattern files (defined below).  The file names
*     are "(cog)progs/image_dither/f_<name>.dith", where <name>
*     stands for the argument entered after the -PATT command line option.
*     See the comment headers in the .dith files indicated above for a
*     description of each dither pattern and an explanation of its purpose.
*     The default is set below.
*
*   -PATT_FNAM filename
*
*     Selects the dither pattern by explicitly giving its file name.
*     The format of dither pattern files is explained below.  The default
*     is a pre-defined dither pattern.   See the description for the -PATT
*     command line option, above.
*
*   Dither pattern files are regular text files that can be examined and
*   modified by normal text editors.  Blank lines are ignored, and "/*"
*   starts a end of line comment.  In other words, "/*" and
*   all characters following it on the same line are ignored.  All keywords
*   are case-insentitive.  Tokens are delimited with blanks.  Redundant blanks
*   are ignored.  Tokens may be enclosed in matching quotes ("") or apostrophies
*   ('').  This is required if the token contains one or more blanks.
*
*   Dither pattern files contain commands, which may be followed by arguments.
*   Each command must start on a new line.  Commands must be completely
*   contained on one line, except as noted.  The valid commands are:
*
*   SIZE nx ny
*
*     Declares the dither pattern size in pixels.  The dither pattern will be
*     a repeating array of tiles, each NX x NY pixels in size.  The top left
*     pixel in the image will be aligned with the top left corner of the
*     dither pattern.  This command must be present, and must preceed the
*     THRESHOLDS command.
*
*   RANGE_THRESH min max
*
*     Declares the range of threshold values used in the THRESHOLDS command.
*     The default is 0 to NX*NY (from the SIZE command).
*     If present, this command must preceed the THRESHOLDS command.
*
*   LEVELS n
*
*     Declares the number of distinct threshold levels.  This is used
*     to determine the total number of apparent levels in the output image.
*     The total apparent levels is (output levels per pixel - 1)*N + 1.
*     The default is NX*NY (from the SIZE command).
*
*   THRESHOLDS
*
*     Indicates the threshold values for each pixel in the dither pattern.
*     When dithering any one pixel, a choice must be made between two possible
*     adjacent output values.
*
*     First, the bias value is computed, which indicates
*     where the incoming pixel value falls within the range of the two possible
*     output pixel value choices.  A bias value of 0.0 indicates that the
*     incoming pixel value is exactly the same as the lower of the two output
*     pixel value choices.   A value of 1.0 indicates it is the same as the
*     higher of the two output choices.
*
*     The bias value is then compared to the dither threshold value for that
*     pixel.  The higher of the two possible output levels is chosen if the
*     bias value meets or exceeds the threshold value.  For purposes of this
*     comparison, the threshold value is normalized to the 0.0 to 1.0 range,
*     based on the MIN and MAX arguments to the RANGE_THRESH command.
*
*     The threshold values are given on the lines following the THRESHOLDS
*     command.  Each subsequent line corresponds to one row in the dither
*     pattern.  Therefore, there must be NY lines following, each containing
*     NX numbers.
*
*     For evenly spaced dither thresholds, the 0.0 to 1.0 normalized thresholds
*     should start at 1/N and go to 1.  For example, for a simple 2x2 dither
*     pattern, the normalized thresholds would be .25, .5, .75, and 1.
}
program image_dither;
%include 'sys.ins.pas';
%include 'util.ins.pas';
%include 'string.ins.pas';
%include 'file.ins.pas';
%include 'img.ins.pas';

const
  max_msg_parms = 2;                   {max parameters we can pass to messages}
  threshold_max = 10000;               {represents normalized 1.0 threshold value}
  predef_patt_name = 'diag5';          {default dither pattern name}

type

  threshold_t = sys_int_conv16_t;      {one threshold or bias value}

  threshold_p_t = ^threshold_t;        {pointer to one threshold value}

  table_entry_t = record               {one mapping table entry}
    high: 0..255;                      {value selected when >= threshold}
    low: 0..255;                       {value selected when < threshold}
    bias: threshold_t;                 {bias value within LOW to HIGH range}
    end;

  table_col_t =                        {mapping table for one color}
    array[0..255] of table_entry_t;

var
  fnam_in,                             {input file name}
  fnam_out,                            {output file name}
  fnam_dith,                           {dither pattern file name}
  predef_dith_name:                    {pathname prefix for predefined pattern files}
    %include '(cog)lib/string_treename.ins.pas';
  name_patt:                           {predefined patt name, empty = use FNAM_DITH}
    %include '(cog)lib/string32.ins.pas';
  img_in: img_conn_t;                  {handle to input image}
  img_out: img_conn_t;                 {handle to output image}
  buf:                                 {sratch large text buffer}
    %include '(cog)lib/string8192.ins.pas';
  parm:                                {command line option parameter}
    %include '(cog)lib/string_treename.ins.pas';
  opt:                                 {command line option name}
    %include '(cog)lib/string32.ins.pas';
  pick: sys_int_machine_t;             {com line option number picked from list}

  lev_red, lev_grn, lev_blu:           {number of output levels for each color}
    sys_int_machine_t;
  table_red, table_grn, table_blu:     {mapping tables for incoming values}
    table_col_t;
  scan_in_p: img_pixel1_p_t;           {pointer to first pixel in input scan line}
  scan_out_p: img_pixel1_p_t;          {pointer to first pixel in output scan line}
  pix_in_p: img_pixel1_p_t;            {pointer to current input pixel}
  pix_out_p: img_pixel1_p_t;           {pointer to current output pixel}
  dith_first_p: threshold_p_t;         {pointer to first dither threshold value}
  dith_scan_p: threshold_p_t;          {pointer to start of thresholds scan line}
  dith_p: threshold_p_t;               {pointer to current dither threshold value}
  dsize_x, dsize_y: sys_int_machine_t; {size of dither pattern in pixels}
  dleft_x: sys_int_machine_t;          {pixels left before wrap dither line}
  dleft_y: sys_int_machine_t;          {lines left before wrap dither pattern}
  x, y: sys_int_machine_t;             {pixel coordinate loop counters}

  stat: sys_err_t;                     {completion status code}
  msg_parm:                            {parameter references for messages}
    array[1..max_msg_parms] of sys_parm_msg_t;

label
  next_opt, done_opts;
{
**********************************
*
*   Local subroutine GET_LINE (CONN, BUF, EOF)
*
*   Read next line from dither pattern file into BUF.  This routine takes care
*   of comments and blank lines.
}
procedure get_line (
  in out  conn: file_conn_t;           {connection handle to input file}
  in out  buf: univ string_var_arg_t;  {returned output buffer}
  out     eof: boolean);               {TRUE if encountered end of file}
  val_param;

var
  i: string_index_t;                   {index to start of comment, if any}
  comment: static string_var4_t :=     {end of line comment start string}
    [str := '/*', len := 2, max := sizeof(comment.str)];
  stat: sys_err_t;

label
  try_again;

begin
  eof := false;                        {init to not hit end of file}

try_again:                             {back here if on blank line}
  file_read_text (conn, buf, stat);    {read next line from file}
  if file_eof(stat) then begin
    eof := true;
    return;
    end;
  sys_error_abort (stat, 'file', 'read_input_text', nil, 0);
  string_unpad (buf);                  {strip trailing blanks}
  string_find (comment, buf, i);       {get start of comment, if exists}
  if i > 0 then begin                  {found comment ?}
    buf.len := i - 1;                  {truncate just before comment}
    string_unpad (buf);                {strip trailing blanks}
    end;
  if buf.len <= 0 then goto try_again; {nothing worthwhile on this line ?}
  end;
{
**********************************
*
*   Local subroutine READ_DITH (FNAM)
*
*   Read and process the dither pattern file FNAM.  FNAM will be returned
*   the full treename of the actual file.
*
*   The following variables will be set in the main program:
*
*     TABLE_RED, TABLE_GRN, TABLE_BLU
*     DSIZE_X, DSIZE_Y
*     DITH_FIRST_P
}
procedure read_dith (
  in out  fnam: string_treename_t);
  val_param;

var
  conn: file_conn_t;                   {connection handle to input file}
  p: string_index_t;                   {parse index into current input line}
  cmd: string_var32_t;                 {command name}
  pick: sys_int_machine_t;             {number of token picked from list}
  tmin, tmax: real;                    {min/max threshold values range}
  levels: sys_int_machine_t;           {number of threshold levels}
  dith_p: threshold_p_t;               {points to current threshold value}
  i, j: sys_int_machine_t;             {scratch integers and loop counters}
  lred, lgrn, lblu: sys_int_machine_t; {total apparent levels for each color}
  r: real;                             {scratch floating point number}
  size_given: boolean;                 {TRUE if processed SIZE command}
  range_thresh_given: boolean;         {TRUE if RANGE_THRESH command found}
  levels_given: boolean;               {TRUE if LEVELS command found}
  eof: boolean;                        {TRUE if reached end of input file}

label
  next_line, done_file;
{
********
*
*   Local subroutine ERROR (SUBSYS,MSG,PARMS,N_PARMS)
*   This subroutine is local to READ_DITH.
*
*   Write indicated error message and then write message identifying offending
*   line number and file name.
}
procedure error (
  in      subsys: string;              {subsystem name}
  in      msg: string;                 {message name within subsystem}
  in      parms: univ sys_parm_msg_ar_t; {array of parameter descriptors}
  in      n_parms: sys_int_machine_t); {number of parameters in PARMS}
  options (val_param, noreturn);

begin
  sys_message_parms (subsys, msg, parms, n_parms);
  sys_msg_parm_int (msg_parm[1], conn.lnum);
  sys_msg_parm_vstr (msg_parm[2], conn.tnam);
  sys_message_bomb ('img', 'image_dither_lnum_fnam', msg_parm, 2);
  end;
{
********
*
*   Local subroutine ERROR_ABORT (STAT,SUBSYS,MSG,PARMS,N_PARMS)
*
*   Write error messages and abort if STAT is indicating an abnormal condition.
}
procedure error_abort (
  in      stat: sys_err_t;             {error status code}
  in      subsys: string;              {subsystem name}
  in      msg: string;                 {message name within subsystem}
  in      parms: univ sys_parm_msg_ar_t; {array of parameter descriptors}
  in      n_parms: sys_int_machine_t); {number of parameters in PARMS}
  val_param;

begin
  if not sys_error(stat) then return;  {no error}
  error (subsys, msg, parms, n_parms);
  end;
{
********
*
*   Start of subroutine READ_DITH.
}
begin
  cmd.max := sizeof(cmd.str);          {init local var string}

  file_open_read_text (fnam, '.dith', conn, stat);
  sys_msg_parm_vstr (msg_parm[1], fnam);
  sys_error_abort (stat, 'file', 'open_input_read_text', msg_parm, 1);
  string_copy (conn.tnam, fnam);       {return full treename of actual file}
{
*   Init defaults before processing commands.
}
  size_given := false;
  range_thresh_given := false;
  levels_given := false;

next_line:                             {back here each new line from the file}
  get_line (conn, buf, eof);           {read next line, comments stripped}
  if eof then begin
    error ('img', 'image_dither_eof', nil, 0);
    end;
  p := 1;                              {init parse index into new line}
  string_token (buf, p, cmd, stat);    {get command name}
  string_upcase (cmd);
  string_tkpick80 (cmd,
    'SIZE RANGE_THRESH LEVELS THRESHOLDS',
    pick);
  case pick of
{
*   SIZE nx ny
}
1: begin
  string_token_int (buf, p, dsize_x, stat);
  error_abort (stat, '', '', nil, 0);
  string_token_int (buf, p, dsize_y, stat);
  error_abort (stat, '', '', nil, 0);
  size_given := true;
  end;
{
*   RANGE_THRESH min max
}
2: begin
  string_token_fpm (buf, p, tmin, stat);
  error_abort (stat, '', '', nil, 0);
  string_token_fpm (buf, p, tmax, stat);
  error_abort (stat, '', '', nil, 0);
  range_thresh_given := true;
  end;
{
*   LEVELS n
}
3: begin
  string_token_int (buf, p, levels, stat);
  error_abort (stat, '', '', nil, 0);
  levels_given := true;
  end;
{
*   THRESHOLDS
}
4: begin
  string_token (buf, p, cmd, stat);
  if not string_eos(stat) then begin
    sys_msg_parm_vstr (msg_parm[1], cmd);
    error ('img', 'image_dither_too_many_tokens', msg_parm, 1);
    end;
  if not size_given then begin         {no previous SIZE command ?}
    error ('img', 'image_dither_size_not_found', nil, 0);
    end;
  if not range_thresh_given then begin {default RANGE_THRESH values ?}
    tmin := 0.0;
    tmax := dsize_x * dsize_y;
    end;
  if not levels_given then begin       {default LEVELS value ?}
    levels := dsize_x * dsize_y;
    end;
  img_mem_alloc (                      {allocate memory for thresholds table}
    img_in,                            {handle to connection to alloc memory under}
    sizeof(dith_p^) * dsize_x * dsize_y, {amount of memory to allocate}
    dith_first_p);                     {returned pointer to start of new memory}
  dith_p := dith_first_p;              {init curr threshold to first in list}
  for i := 1 to dsize_y do begin       {once for each line of threshold values}
    get_line (conn, buf, eof);         {read next line of threshold values}
    if eof then begin
      error ('img', 'image_dither_eof', nil, 0);
      end;
    p := 1;                            {init parse index for this line}
    for j := 1 to dsize_x do begin     {once for each threshold value this line}
      string_token_fpm (buf, p, r, stat); {get threshold value}
      error_abort (stat, '', '', nil, 0);
      r := (r - tmin) / (tmax - tmin); {normalize to 0.0 to 1.0 range}
      r := r * (threshold_max + 1);    {scale to final range it will be stored in}
      r := max(0.0, min(threshold_max-0.5, r));
      dith_p^ := trunc(r);             {save final value in threshold table}
      dith_p := univ_ptr(              {advance pointer to next threshold value}
        sys_int_adr_t(dith_p) + sizeof(dith_p^));
      end;                             {back for next threshold this line}
    string_token (buf, p, cmd, stat);
    if not string_eos(stat) then begin
      sys_msg_parm_vstr (msg_parm[1], cmd);
      error ('img', 'image_dither_too_many_tokens', msg_parm, 1);
      end;
    end;                               {back for next line of threshold values}
  get_line (conn, buf, eof);           {try to read another line from file}
  if not eof then begin                {more stuff after THRESHOLDS command ?}
    error ('img', 'image_dither_too_many_lines', nil, 0);
    end;
  file_close (conn);                   {all done with dither pattern file}
  goto done_file;
  end;
{
*   Unrecognized command.
}
otherwise
    sys_msg_parm_vstr (msg_parm[1], cmd);
    error ('img', 'image_dither_cmd_bad', msg_parm, 1);
    end;
{
*   Just finished processing the command on the current line.
}
  string_token (buf, p, cmd, stat);
  if not string_eos(stat) then begin
    sys_msg_parm_vstr (msg_parm[1], cmd);
    error ('img', 'image_dither_too_many_tokens', msg_parm, 1);
    end;
  goto next_line;                      {back and process next dither file line}
{
*   All done with the dither pattern file.
}
done_file:
  lred := ((lev_red-1) * levels) + 1;  {total apparent levels for each color}
  lgrn := ((lev_grn-1) * levels) + 1;
  lblu := ((lev_blu-1) * levels) + 1;
  for i := 0 to 255 do begin           {once for each possible input pixel value}
    j := (i * lred) div (255 * levels); {number of range we are within}
    j := min(j, lev_red-2);            {clip to highest existing range}
    table_red[i].low :=                {output 0-255 level for LOW choice}
      (j * 256) div (lev_red - 1);
    table_red[i].high :=               {output 0-255 level for HIGH choice}
      min(255, ((j+1) * 256) div (lev_red - 1));
    table_red[i].bias :=               {scaled bias value within this range}
      ((i * lred * threshold_max) - (255 * j * levels * threshold_max)) div
      (levels * 255);

    j := (i * lgrn) div (255 * levels); {number of range we are within}
    j := min(j, lev_grn-2);            {clip to highest existing range}
    table_grn[i].low :=                {output 0-255 level for LOW choice}
      (j * 256) div (lev_grn - 1);
    table_grn[i].high :=               {output 0-255 level for HIGH choice}
      min(255, ((j+1) * 256) div (lev_grn - 1));
    table_grn[i].bias :=               {scaled bias value within this range}
      ((i * lgrn * threshold_max) - (255 * j * levels * threshold_max)) div
      (levels * 255);

    j := (i * lblu) div (255 * levels); {number of range we are within}
    j := min(j, lev_blu-2);            {clip to highest existing range}
    table_blu[i].low :=                {output 0-255 level for LOW choice}
      (j * 256) div (lev_blu - 1);
    table_blu[i].high :=               {output 0-255 level for HIGH choice}
      min(255, ((j+1) * 256) div (lev_blu - 1));
    table_blu[i].bias :=               {scaled bias value within this range}
      ((i * lblu * threshold_max) - (255 * j * levels * threshold_max)) div
      (levels * 255);
    end;                               {back for next input intensity value}
  end;
{
**********************************
*
*   Start of main routine.
}
begin
  string_cmline_init;                  {init for processing command line}

  sys_cognivis_dir ('progs/image_dither/f_', predef_dith_name);

  string_cmline_token (fnam_in, stat); {get input file name from command line}
  string_cmline_req_check (stat);

  img_open_read_img (fnam_in, img_in, stat); {open input image for read}
  sys_msg_parm_vstr (msg_parm[1], fnam_in);
  sys_error_abort (stat, 'img', 'open_read', msg_parm, 1);
{
*   Set defaults before processing command line options.
}
  lev_red := 6;
  lev_grn := 7;
  lev_blu := 6;
  string_copy (img_in.gnam, fnam_out);
  string_vstring (name_patt, predef_patt_name, sizeof(predef_patt_name));
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
    '-OUT -BITS -LEVELS -PATT -PATT_FNAM', {valid option names}
    pick);                             {number of picked option}
  case pick of                         {do routine for specific option}
{
*   -OUT <output file name>
}
1: begin
  string_cmline_token (fnam_out, stat);
  end;
{
*   -BITS r g b
}
2: begin
  string_cmline_token_int (lev_red, stat);
  string_cmline_parm_check (stat, opt);
  if lev_red < 1 then begin
    sys_msg_parm_int (msg_parm[1], lev_red);
    sys_message_bomb ('img', 'image_dither_bits_bad', msg_parm, 1);
    end;
  lev_red := lshft(1, lev_red);

  string_cmline_token_int (lev_grn, stat);
  string_cmline_parm_check (stat, opt);
  if lev_grn < 1 then begin
    sys_msg_parm_int (msg_parm[1], lev_grn);
    sys_message_bomb ('img', 'image_dither_bits_bad', msg_parm, 1);
    end;
  lev_grn := lshft(1, lev_grn);

  string_cmline_token_int (lev_blu, stat);
  string_cmline_parm_check (stat, opt);
  if lev_blu < 1 then begin
    sys_msg_parm_int (msg_parm[1], lev_blu);
    sys_message_bomb ('img', 'image_dither_bits_bad', msg_parm, 1);
    end;
  lev_blu := lshft(1, lev_blu);
  end;
{
*   -LEVELS r g b
}
3: begin
  string_cmline_token_int (lev_red, stat);
  string_cmline_parm_check (stat, opt);
  if lev_red < 2 then begin
    sys_msg_parm_int (msg_parm[1], lev_red);
    sys_message_bomb ('img', 'image_dither_levels_bad', msg_parm, 1);
    end;

  string_cmline_token_int (lev_grn, stat);
  string_cmline_parm_check (stat, opt);
  if lev_grn < 2 then begin
    sys_msg_parm_int (msg_parm[1], lev_grn);
    sys_message_bomb ('img', 'image_dither_levels_bad', msg_parm, 1);
    end;

  string_cmline_token_int (lev_blu, stat);
  string_cmline_parm_check (stat, opt);
  if lev_blu < 2 then begin
    sys_msg_parm_int (msg_parm[1], lev_blu);
    sys_message_bomb ('img', 'image_dither_levels_bad', msg_parm, 1);
    end;
  end;
{
*   -PATT name
}
4: begin
  string_cmline_token (name_patt, stat);
  string_cmline_parm_check (stat, opt);
  string_downcase (name_patt);
  if name_patt.len <= 0 then begin     {generic pattern name is empty ?}
    string_vstring (                   {substitute default pattern name}
     name_patt, predef_patt_name, sizeof(predef_patt_name));
    end;
  end;
{
*   -PATT_FNAM filename
}
5: begin
  string_cmline_token (fnam_dith, stat);
  name_patt.len := 0;                  {indicate FNAM_DITH all set}
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
*   Read and process the dither pattern file.  This will set the TABLE_RED,
*   TABLE_GRN, and TABLE_BLU data structures, among other things.
}
  if name_patt.len > 0 then begin      {we only have generic pattern name ?}
    string_copy (predef_dith_name, fnam_dith); {init with static part of pathname}
    string_append (fnam_dith, name_patt); {add generic pattern name}
    end;

  read_dith (                          {read dither pattern file}
    fnam_dith);                        {will be returned full treename of dith file}
{
*   Set up the output image file.
}
  string_list_pos_last (img_in.comm);  {move to last comment line}
  string_list_line_add (img_in.comm);  {create comment line for date}
  sys_date_time1 (img_in.comm.str_p^); {init our comment line with date/time string}
  string_appends (img_in.comm.str_p^, '  Dithered from file:');

  string_list_line_add (img_in.comm);  {create comment line for file name}
  string_appendn (img_in.comm.str_p^, '  ', 2);
  string_append (img_in.comm.str_p^, img_in.tnam);

  string_list_line_add (img_in.comm);  {create comment line for comline parms}
  string_appendn (img_in.comm.str_p^, '  ', 2);
  string_appends (img_in.comm.str_p^, 'Using IMAGE_DITHER -LEVELS '(0));
  string_f_int (parm, lev_red);
  string_append (img_in.comm.str_p^, parm);
  string_append1 (img_in.comm.str_p^, ' ');
  string_f_int (parm, lev_grn);
  string_append (img_in.comm.str_p^, parm);
  string_append1 (img_in.comm.str_p^, ' ');
  string_f_int (parm, lev_blu);
  string_append (img_in.comm.str_p^, parm);
  if name_patt.len >= 0
    then begin                         {using pre-defined pattern}
      string_appends (img_in.comm.str_p^, ' -PATT '(0));
      string_append (img_in.comm.str_p^, name_patt);
      end
    else begin                         {using user-supplied dither pattern}
      string_appends (img_in.comm.str_p^, '.  Dither pattern defined in file:'(0));
      string_list_line_add (img_in.comm);
      string_appendn (img_in.comm.str_p^, '  ', 2);
      string_append (img_in.comm.str_p^, fnam_dith);
      end
    ;

  string_appendn (buf, 'ALPHA ', 6);   {make output file format request string}
  string_f_int (parm, img_in.bits_alpha);
  string_append (buf, parm);
  string_appends (buf, ' RED 8 GREEN 8 BLUE 8');

  img_open_write_img (                 {open output image for write}
    fnam_out,                          {output file name}
    img_in.aspect,                     {aspect ratio of properly displayed image}
    img_in.x_size, img_in.y_size,      {size of output image in pixels}
    '',                                {select no explicit file type}
    buf,                               {output file special requests}
    img_in.comm,                       {comments list}
    img_out,                           {new image connection handle}
    stat);
  sys_msg_parm_vstr (msg_parm[1], fnam_out);
  sys_error_abort (stat, 'img', 'open_write', msg_parm, 1);
{
*   Initialize before the main loop.
}
  img_mem_alloc (                      {allocate input scan line buffer}
    img_in,
    sizeof(scan_in_p^) * img_in.x_size,
    scan_in_p);
  img_mem_alloc (                      {allocate output scan line buffer}
    img_out,
    sizeof(scan_out_p^) * img_out.x_size,
    scan_out_p);
  dith_scan_p := dith_first_p;         {init vertical dither pattern phase}
  dleft_y := dsize_y;
{
*   Do the dithering.
}
  for y := 0 to img_in.y_size-1 do begin {down the scan lines}
    img_read_scan1 (img_in, scan_in_p^, stat); {read input scan line}
    sys_error_abort (stat, 'img', 'read_scan_line', nil, 0);
    pix_in_p := scan_in_p;             {init curr input pixel to first in scan line}
    pix_out_p := scan_out_p;           {init curr output pixel to first in out scan}
    dith_p := dith_scan_p;             {init horizontal dither pattern phase}
    dleft_x := dsize_x;

    for x := 0 to img_in.x_size-1 do begin {across this scan line}
      pix_out_p^.alpha := pix_in_p^.alpha; {copy alpha value without modification}
      with table_red[pix_in_p^.red]: entry do begin
        if entry.bias >= dith_p^
          then pix_out_p^.red := entry.high
          else pix_out_p^.red := entry.low;
        end;
      with table_grn[pix_in_p^.grn]: entry do begin
        if entry.bias >= dith_p^
          then pix_out_p^.grn := entry.high
          else pix_out_p^.grn := entry.low;
        end;
      with table_blu[pix_in_p^.blu]: entry do begin
        if entry.bias >= dith_p^
          then pix_out_p^.blu := entry.high
          else pix_out_p^.blu := entry.low;
        end;
      pix_in_p := univ_ptr(            {advance to next input pixel}
        sys_int_adr_t(pix_in_p) + sizeof(pix_in_p^));
      pix_out_p := univ_ptr(           {advance to next output pixel}
        sys_int_adr_t(pix_out_p) + sizeof(pix_out_p^));
      dleft_x := dleft_x - 1;
      if dleft_x > 0
        then begin                     {advance to next dither value accross}
          dith_p := univ_ptr(
            sys_int_adr_t(dith_p) + sizeof(dith_p^));
          end
        else begin                     {wrap back to start of this dither row}
          dith_p := dith_scan_p;
          dleft_x := dsize_x;
          end
        ;
      end;                             {back and do next pixel this scan line}

    img_write_scan1 (img_out, scan_out_p^, stat); {write output scan line to file}
    sys_error_abort (stat, 'img', 'write_scan_line', nil, 0);
    dleft_y := dleft_y - 1;
    if dleft_y > 0
      then begin                       {advance to next scan line in dither pattern}
        dith_scan_p := univ_ptr(sys_int_adr_t(dith_scan_p) +
          (sizeof(dith_scan_p^) * dsize_x) );
        end
      else begin                       {wrap back to top of dither pattern}
        dith_scan_p := dith_first_p;
        dleft_y := dsize_y;
        end
      ;
    end;                               {back and do next scan line in image}

  img_close (img_in, stat);            {close image input file}
  sys_error_print (stat, 'img', 'close', nil, 0);
  img_close (img_out, stat);           {close image output file}
  sys_error_print (stat, 'img', 'close', nil, 0);
  end.
