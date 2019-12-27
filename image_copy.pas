{   Program COPY_IMAGE <in file name> [ options ]
*
*   Copy an image from the input file to the output file.
*   Valid command line options are:
*
*   <output file name>
*
*     Explicitly set the generic output file name.  The real output file name
*     always has a particular suffix appended, depending on the output file type.
*     The suffix is permitted on the command line argument, and will not appear
*     twice in the real output file name.  The default output file name is the
*     generic leaf name of the input file.  This is useful when the input file
*     is being copied from another directory.  This must be the first command
*     line option, if present.
*
*   -FTYPE <image file type name>
*
*     Select the image output file type.  The valid choices are the names of
*     all the available image file I/O drivers.  Currently, the drivers
*     IMG, TGA, and BMF come standard.  These names also correspond to the
*     file name suffixes, although the file name suffixes are always lower case,
*     where the <image file type name> parameter is case-insensitive.
*     The default is IMG.
*
*   -FORM <format string>
*
*     Pass specific format directives to the driver used for writing the output
*     file.  The meaning of the format string is interpreted separately by
*     each driver.  The format string must be enclosed in quotes if it contains
*     any spaces.
*
*   -ALPHA [<bits precision>]
*   -RED [<bits precision>]
*   -GRN [<bits precision>]
*   -BLU [<bits precision>]
*
*     Declare the minimum desired bits of precision per pixel for each of the
*     alpha, red, green, or blue pixel components.  This will be done if
*     possible, but is not guaranteed.  The default precisions when these
*     command line options are not used are the precisions used in the input
*     file.  When these command line options are used without the <bits precision>
*     parameter, then <bits precision> defaults to 8 if the component did
*     not exist (precision = 0) in the input file and the -BITS command line
*     option is not used.
*
*     These command line options without the <bits precision> parameter are
*     intended for declaring the existance of pixel components while otherwise
*     effecting the precision as little as possible.
*
*   -BITS <bits precision>
*
*     Set the precision for all pixel components that would otherwise be set
*     to more than zero, and are not explicitly set.  This option is intended
*     as an easy way to chose between output file formats of different precisions
*     without effecting which pixel components exist.  This will also be the
*     precision default for any component precision command line option without
*     the <bits precision> parameter.
*
*   -BW
*
*     Convert the output image to black and white.
*
*   The remaining command line options control a filter that can adjust the
*   red, green, and blue values in each pixel.  Conceptually, there are 3 stages
*   to this filter.  In order, they are a linear transform, non-linear brightness and
*   saturation adjustments, and another linear transform.
*
*   Parameters for stage 1 (input linear transform) are:
*
*     -RANGE     <black level> <white level>
*     -RANGE_RED <black level> <white level>
*     -RANGE_GRN <black level> <white level>
*     -RANGE_BLU <black level> <white level>
*
*       Select the input image levels that will be passed to stage 2 as the
*       full black and full white levels.  The input image values are in a 0.0
*       to 1.0 scale.  The -RANGE command line option sets the values for
*       all three color components the same.
*
*       The stage 1 transform is most useful for images that do not make use
*       of the whole 0.0 to 1.0 available range, or that do not have red green and
*       blue mapped to the same range.  This is typical of images that are digitized
*       as apposed to computer generated.  The program IMAGE_HIST may be useful
*       to determine what the black to white range should be.  The format of the
*       numbers here is intentionally the same format as the numbers in column
*       one of a histogram from IMAGE_HIST.
*
*       The default is to not alter the color ranges (black level = 0.0,
*       white level = 1.0).
*
*   Parameters for stage 2 (non-linear brightnes add saturation adjust) are:
*
*     -BRIGHTEN f
*
*       Brighten or darken the image while not altering the ends of the range.
*       Positive values will brighten the image, while negative values will
*       darken it.  Useful values are typically in the range of -1 to 1.
*       The default is to not alter the image values (F = 0.0).
*
*     -SATURATE f
*
*       Adjust the relative saturation of color values.  This adjustment will have
*       no effect on fully desaturated (gray) or fully saturated colors.
*       Intermediate colors will be more saturated when F is positive and less
*       saturated when F is negative.  Useful values are typically in the range
*       of -1 to 1.  The default is to not alter the saturation (F = 0.0).
*
*   Parameters for stage 3 (output linear transform) are:
*
*     -OFS_LOW f
*
*       Add an offset to the bottom (black) end of the color range.  The image color
*       values will be transformed from the range (0.0 --> 1.0) to (F --> 1.0).  The
*       default is to not alter the black color value (F = 0.0).
*
*     -OFS_HIGH f
*
*       Add an offset to the top (white) end of the color range.  The image color
*       values will be transformed from the range (0.0 --> 1.0) to (0.0 --> 1.0+F).
*       The default is to not alther the white color value (F = 0.0).
}
program image_copy;
%include 'sys.ins.pas';
%include 'util.ins.pas';
%include 'string.ins.pas';
%include 'file.ins.pas';
%include 'img.ins.pas';

const
  max_msg_args = 8;                    {max arguments we can pass to a message}
  n_cmds = 16;                         {number of possible commands}
  cmd_len_max = 10;                    {size of largest command name}
  bw_red = 0.299;                      {mult factors for black and white conversion}
  bw_grn = 0.587;
  bw_blu = 0.114;

  cmd_len_alloc = cmd_len_max + 1;     {chars to allocate for each command}
  cmds_len = cmd_len_alloc * n_cmds;   {total number of chars in commands list}
  bw_ired = round(bw_red * 256.0);     {integer BW conversion mult factors}
  bw_igrn = round(bw_grn * 256.0);
  bw_iblu = round(bw_blu * 256.0);

type
  cmd_t =                              {one command name}
    array[1..cmd_len_alloc] of char;

  cmds_t = record                      {array of all the command names}
    max: string_index_t;               {simulate a var string}
    len: string_index_t;
    str: array[1..n_cmds] of cmd_t;
    end;

  range_t = record                     {descriptor set of one -RANGE values}
    low: real;                         {input value that should map to 0.0}
    high: real;                        {input value that should map to 1.0}
    end;

var
  in_fnam,                             {input file name}
  out_fnam:                            {output file name}
    %include '(cog)lib/string_treename.ins.pas';
  ftype:                               {output file type name}
    %include '(cog)lib/string80.ins.pas';
  uform:                               {format string requested by user}
    %include '(cog)lib/string_treename.ins.pas';
  form:                                {total format string for output driver}
    %include '(cog)lib/string8192.ins.pas';
  bits_alpha: sys_int_machine_t;       {min requested component precision}
  bits_red: sys_int_machine_t;
  bits_grn: sys_int_machine_t;
  bits_blu: sys_int_machine_t;
  set_alpha: boolean;                  {TRUE if component precisions explicitly set}
  set_red: boolean;
  set_grn: boolean;
  set_blu: boolean;
  bits_all: sys_int_machine_t;         {requested precision for all components}
  set_all: boolean;                    {TRUE if -BITS command line option found}
  bw: boolean;                         {TRUE if -BW command line option found}
  saturate: real;                      {command line SATURATE factor}
  brighten: real;                      {command line BRIGHTEN factor}
  ofs_low: real;
  ofs_high: real;
  range_red: range_t;
  range_grn: range_t;
  range_blu: range_t;
  mult, ofs: real;                     {scratch for dealing with linear ranges}

  img_in: img_conn_t;                  {handle to input image}
  img_inf: img_conn_t;                 {handle to read input thru filter}
  img_in_p: img_conn_p_t;              {points to input handle to actually use}
  img_out: img_conn_t;                 {handle to output image}
  scan_p: img_scan2_arg_p_t;           {pointer to memory for one scan line}
  scan_size: sys_int_adr_t;            {memory size of scan line buffer}
  pix_p: img_pixel2_p_t;
  y: sys_int_machine_t;                {scan line loop counter}
  x: sys_int_machine_t;                {current X pixel coordinate}
  ii: sys_int_machine_t;               {scratch integer}
  msg_parms:                           {references arguments passed to a message}
    array[1..max_msg_args] of sys_parm_msg_t;
  stat: sys_err_t;                     {completion status code}

  pick: sys_int_machine_t;             {number of token picked from list}
  opt:                                 {upcased command line option}
    %include '(cog)lib/string_treename.ins.pas';
  parm:                                {command line option parameter}
    %include '(cog)lib/string_treename.ins.pas';
  cmds: cmds_t := [                    {all the command names}
    max := cmds_len, len := cmds_len, str := [
      '-FTYPE    ',                    {1}
      '-FORM     ',                    {2}
      '-ALPHA    ',                    {3}
      '-RED      ',                    {4}
      '-GRN      ',                    {5}
      '-BLU      ',                    {6}
      '-BITS     ',                    {7}
      '-SATURATE ',                    {8}
      '-BRIGHTEN ',                    {9}
      '-OFS_LOW  ',                    {10}
      '-OFS_HIGH ',                    {11}
      '-RANGE    ',                    {12}
      '-RANGE_RED',                    {13}
      '-RANGE_GRN',                    {14}
      '-RANGE_BLU',                    {15}
      '-BW       ',                    {16}
      ]
    ];

label
  next_opt, done_opts;
{
*******************************************************************
*
*   Local subroutine OPTIONAL_BITS (BITS, BITS_SET, STAT)
*
*   Check the next command line token for being an optional <bits precision>
*   parameter.  The previous command line token was one of the
*   -<component name> options.
}
procedure optional_bits (
  in out  bits: sys_int_machine_t;     {bits precision of this component}
  out     bits_set: boolean;           {set to TRUE if BITS explicitly set}
  out     stat: sys_err_t);

var
  parm: string_var32_t;                {next command line parameter}

label
  default;

begin
  parm.max := sizeof(parm.str);        {init var string}

  string_cmline_token (parm, stat);    {try to read next command line token}
  if string_eos(stat) then goto default; {got to end of command line ?}
  if sys_error(stat) then return;      {a real error on reading command line ?}
  if (parm.len >= 1) and (parm.str[1] <> '-') then begin {not the next cmline opt ?}
    string_t_int (parm, bits, stat);   {try to convert token to integer}
    if sys_error(stat) then return;
    bits_set := true;                  {BITS was set to explicit value}
    return;                            {normal return, BITS explicitly set}
    end;
  string_cmline_reuse;                 {that token wasn't for us}

default:                               {<bits precision> is being defaulted}
  if bits <= 0                         {this component otherwise not exist ?}
    then bits := 8;                    {component now definately exists}
  end;                                 {normal return, BITS defaulted}
{
*******************************************************************
*
*   Start of main routine.
}
begin
{
*   Get the input file name.
}
  string_cmline_init;                  {init command line parsing}
  string_cmline_token (in_fnam, stat); {get input image file name}
  if sys_error(stat) then begin
    if string_eos(stat)
      then begin                       {missing}
        sys_message ('string', 'cmline_arg1_missing');
        end
      else begin                       {error}
        sys_error_print (stat, 'string', 'cmline_opt_err', nil, 0);
        end
      ;
    sys_bomb;
    end;
{
*   Get optional output file name.
}
  string_cmline_token (parm, stat);    {read second command line token, if present}
  if sys_error(stat)
    then begin                         {we did NOT read command line token}
      if not string_eos(stat) then begin {a real error ?}
        sys_error_print (stat, 'string', 'cmline_opt_err', nil, 0);
        sys_bomb;
        end;
      end
    else begin                         {PARM contains second command line token}
      if (parm.len >= 1) and (parm.str[1] <> '-')
        then begin                     {PARM is output file name}
          string_copy (parm, out_fnam); {set output file name}
          end
        else begin                     {PARM is command line option}
          string_cmline_reuse;         {read this cmd line token again later}
          end
        ;
      end                              {done handling second cmd line token}
    ;
{
*   Open the input image file.
}
  img_open_read_img (in_fnam, img_in, stat); {try to open image input file}
  sys_msg_parm_vstr (msg_parms[1], in_fnam);
  sys_error_abort (stat, 'img', 'open_read', msg_parms, 1); {abort on error}

  string_list_pos_last (img_in.comm);  {move to last comment line}
  string_list_line_add (img_in.comm);  {create comment line with date}
  sys_date_time1 (img_in.comm.str_p^); {init our comment line with date/time string}
  string_appends (img_in.comm.str_p^, '  Copied from file');

  string_list_line_add (img_in.comm);  {create comment line with file name}
  string_appendn (img_in.comm.str_p^, '  ', 2);
  string_append (img_in.comm.str_p^, img_in.tnam);
{
*   Set defaults before processing command line options.
}
  ftype.len := 0;                      {default to no explicit output file type}
  bits_alpha := img_in.bits_alpha;     {default precision comes from input file}
  bits_red := img_in.bits_red;
  bits_grn := img_in.bits_grn;
  bits_blu := img_in.bits_blu;
  set_alpha := false;                  {init to precisions not explicitly set}
  set_red := false;
  set_grn := false;
  set_blu := false;
  set_all := false;                    {init to global precision not set}
  bw := false;                         {init to output image will be in color}
  saturate := 0.0;                     {default to no changes in pixel values}
  brighten := 0.0;
  ofs_low := 0.0;
  ofs_high := 0.0;
  range_red.low := 0.0;
  range_red.high := 1.0;
  range_grn.low := 0.0;
  range_grn.high := 1.0;
  range_blu.low := 0.0;
  range_blu.high := 1.0;
{
*   Process all the command line options.
}
next_opt:                              {back here each new command line option}
  string_cmline_token (opt, stat);     {read new command line option}
  if string_eos(stat) then goto done_opts; {nothing left on command line ?}
  sys_error_abort (stat, 'string', 'cmline_opt_err', nil, 0);
  string_upcase (opt);                 {make upper case for token matching}
  string_tkpick (opt, cmds, pick);     {pick option from list of choices}
  case pick of
{
*   -FTYPE <image file type name>
}
1: begin
  string_cmline_token (ftype, stat);
  string_upcase (ftype);
  end;
{
*   -FORM <format string>
}
2: begin
  string_cmline_token (uform, stat);
  end;
{
*   -ALPHA [<bits precision>]
}
3: begin
  optional_bits (bits_alpha, set_alpha, stat);
  end;
{
*   -RED [<bits precision>]
}
4: begin
  optional_bits (bits_red, set_red, stat);
  end;
{
*   -GRN [<bits precision>]
}
5: begin
  optional_bits (bits_grn, set_grn, stat);
  end;
{
*   -BLU [<bits precision>]
}
6: begin
  optional_bits (bits_blu, set_blu, stat);
  end;
{
*   -BITS <bits precision>
}
7: begin
  string_cmline_token_int (bits_all, stat);
  string_cmline_parm_check (stat, opt);
  set_all := true;
  end;
{
*   -SATURATE f
}
8: begin
  string_cmline_token_fpm (saturate, stat);
  end;
{
*   -BRIGHTEN f
}
9: begin
  string_cmline_token_fpm (brighten, stat);
  end;
{
*   -OFS_LOW f
}
10: begin
  string_cmline_token_fpm (ofs_low, stat);
  end;
{
*   -OFS_HIGH f
}
11: begin
  string_cmline_token_fpm (ofs_high, stat);
  end;
{
*   -RANGE low high
}
12: begin
  string_cmline_token_fpm (range_red.low, stat);
  string_cmline_parm_check (stat, opt);
  string_cmline_token_fpm (range_red.high, stat);
  range_grn := range_red;
  range_blu := range_red;
  end;
{
*   -RANGE_RED low high
}
13: begin
  string_cmline_token_fpm (range_red.low, stat);
  string_cmline_parm_check (stat, opt);
  string_cmline_token_fpm (range_red.high, stat);
  end;
{
*   -RANGE_GRN low high
}
14: begin
  string_cmline_token_fpm (range_grn.low, stat);
  string_cmline_parm_check (stat, opt);
  string_cmline_token_fpm (range_grn.high, stat);
  end;
{
*   -RANGE_BLU low high
}
15: begin
  string_cmline_token_fpm (range_blu.low, stat);
  string_cmline_parm_check (stat, opt);
  string_cmline_token_fpm (range_blu.high, stat);
  end;
{
*   -BW
}
16: begin
  bw := true;
  end;
{
*   Unrecognized command line option.
}
otherwise
    sys_msg_parm_vstr (msg_parms[1], opt);
    sys_message_bomb ('string', 'cmline_opt_bad', msg_parms, 1);
    end;                               {done with command line option cases}

  string_cmline_parm_check (stat, opt);
  goto next_opt;
done_opts:                             {jump here on done with command line options}
{
*   All the tokens on the command line have been read.  Now reformat that
*   information into the form we need.
}
  string_fill (ftype);                 {fill with blanks for passing as STRING}

  if out_fnam.len <= 0 then begin      {output file name not explicitly set ?}
    string_copy (img_in.gnam, out_fnam); {default to input file generic leaf name}
    end;

  if set_all then begin                {user set default precision for all comps ?}
    if (not set_alpha) and (bits_alpha > 0)
      then bits_alpha := bits_all;
    if (not set_red) and (bits_red > 0)
      then bits_red := bits_all;
    if (not set_grn) and (bits_grn > 0)
      then bits_grn := bits_all;
    if (not set_blu) and (bits_blu > 0)
      then bits_blu := bits_all;
    end;

  string_appends (form, 'ALPHA');      {write ALPHA resolution into format string}
  string_append1 (form, ' ');
  string_f_int (parm, bits_alpha);
  string_append (form, parm);
  string_append1 (form, ' ');

  string_appends (form, 'RED');        {write RED resolution into format string}
  string_append1 (form, ' ');
  string_f_int (parm, bits_red);
  string_append (form, parm);
  string_append1 (form, ' ');

  string_appends (form, 'GREEN');      {write GREEN resolution into format string}
  string_append1 (form, ' ');
  string_f_int (parm, bits_grn);
  string_append (form, parm);
  string_append1 (form, ' ');

  string_appends (form, 'BLUE');       {write BLUE resolution into format string}
  string_append1 (form, ' ');
  string_f_int (parm, bits_blu);
  string_append (form, parm);

  if uform.len > 0 then begin          {there is a user supplied format string ?}
    string_append1 (form, ' ');
    string_append (form, uform);       {append user string to end of ours}
    end;
{
*   Hook in WARP filter if necessary.
}
  img_in_p := addr(img_in);            {init to no filter in use}
  if                                   {need to use WARP filter ?}
      (range_red.low <> 0.0) or
      (range_red.high <> 1.0) or
      (range_grn.low <> 0.0) or
      (range_grn.high <> 1.0) or
      (range_blu.low <> 0.0) or
      (range_blu.high <> 1.0) or
      (saturate <> 0.0) or
      (brighten <> 0.0) or
      (ofs_low <> 0.0) or
      (ofs_high <> 0.0)
      then begin

    parm.len := 0;                     {init parameter string for filter}

    mult := 1.0 / (range_red.high - range_red.low);
    ofs := -range_red.low * mult;
    string_appends (parm, 'MULT_IN_RED');
    string_append1 (parm, ' ');
    string_f_fp_fixed (opt, mult, 3);
    string_append (parm, opt);
    string_appends (parm, ' ADD_IN_RED');
    string_append1 (parm, ' ');
    string_f_fp_fixed (opt, ofs, 3);
    string_append (parm, opt);

    string_append1 (parm, ' ');
    mult := 1.0 / (range_grn.high - range_grn.low);
    ofs := -range_grn.low * mult;
    string_appends (parm, 'MULT_IN_GRN');
    string_append1 (parm, ' ');
    string_f_fp_fixed (opt, mult, 3);
    string_append (parm, opt);
    string_appends (parm, ' ADD_IN_GRN');
    string_append1 (parm, ' ');
    string_f_fp_fixed (opt, ofs, 3);
    string_append (parm, opt);

    string_append1 (parm, ' ');
    mult := 1.0 / (range_blu.high - range_blu.low);
    ofs := -range_blu.low * mult;
    string_appends (parm, 'MULT_IN_BLU');
    string_append1 (parm, ' ');
    string_f_fp_fixed (opt, mult, 3);
    string_append (parm, opt);
    string_appends (parm, ' ADD_IN_BLU');
    string_append1 (parm, ' ');
    string_f_fp_fixed (opt, ofs, 3);
    string_append (parm, opt);

    string_append1 (parm, ' ');
    string_appends (parm, 'SATURATE');
    string_append1 (parm, ' ');
    string_f_fp_fixed (opt, saturate, 3);
    string_append (parm, opt);

    string_append1 (parm, ' ');
    string_appends (parm, 'BRIGHTEN');
    string_append1 (parm, ' ');
    string_f_fp_fixed (opt, brighten, 3);
    string_append (parm, opt);

    string_append1 (parm, ' ');
    mult := ofs_high + 1.0 - ofs_low;
    string_appends (parm, 'MULT_OUT');
    string_append1 (parm, ' ');
    string_f_fp_fixed (opt, mult, 3);
    string_append (parm, opt);
    string_appends (parm, ' ADD_OUT');
    string_append1 (parm, ' ');
    string_f_fp_fixed (opt, ofs_low, 3);
    string_append (parm, opt);

    img_open_read_filt (               {open connection to WARP filter}
      img_in,                          {source image data stream for filter}
      'WARP',                          {filter name}
      parm,                            {parameter string for filter}
      img_inf,                         {handle for reading filtered data}
      stat);
    sys_error_abort (stat, '', '', nil, 0);

    img_in_p := addr(img_inf);         {switch to reading from filter}
    end;                               {done handling switching in WARP filter}
{
*   Open the image output file.
}
  img_open_write_img (                 {open image output file for write}
    out_fnam,                          {file name}
    img_in_p^.aspect,                  {aspect ratio}
    img_in_p^.x_size, img_in_p^.y_size, {image size in pixels}
    ftype.str,                         {image file type (IMG, TGA, etc.)}
    form,                              {driver format string}
    img_in_p^.comm,                    {pointer to first comment line chain entry}
    img_out,                           {returned image connection handle}
    stat);
  sys_msg_parm_vstr (msg_parms[1], out_fnam);
  sys_error_abort (stat, 'img', 'open_write', msg_parms, 1); {abort on error}
{
*   Copy all the scan lines.
}
  scan_size :=                         {find size of one scan line of pixels}
    sizeof(img_pixel2_t) * img_in_p^.x_size;
  img_mem_alloc (img_out, scan_size, scan_p); {allocate memory for scan line}

  for y := 0 to img_in_p^.y_size-1 do begin {once for each scan line}
    img_read_scan2 (img_in_p^, scan_p^, stat); {read scan line from input image}
    sys_error_abort (stat, 'img', 'read_scan_line', nil, 0);
    if bw then begin                   {convert to black and white ?}
      pix_p := univ_ptr(scan_p);       {init pointer to first pixel}
      for x := 0 to img_in_p^.x_size-1 do begin {accross the scan line}
        ii :=                          {accumulate RGB contributions}
          (pix_p^.red * bw_ired) +
          (pix_p^.grn * bw_igrn) +
          (pix_p^.blu * bw_iblu);
        ii := ii div 256;              {back into orignal pixel range}
        ii := max(0, min(65535, ii));  {clamp to allowable range}
        pix_p^.red := ii;              {stuff gray value back into pixel}
        pix_p^.grn := ii;
        pix_p^.blu := ii;
        pix_p := univ_ptr(             {advance to next pixel}
          sys_int_adr_t(pix_p) + sizeof(pix_p^));
        end;                           {back for next pixel in scan line}
      end;                             {done with BW conversion}
    img_write_scan2 (img_out, scan_p^, stat); {write scan line to output image}
    sys_error_abort (stat, 'img', 'write_scan_line', nil, 0);
    end;                               {back and do next scan line}
{
*   All the scan lines have been copied from the input image to the output image.
}
  img_close (img_in_p^, stat);         {close input image}
  sys_msg_parm_vstr (msg_parms[1], img_in_p^.tnam);
  sys_error_print (stat, 'img', 'close', msg_parms, 1);

  img_close (img_out, stat);           {close output image}
  sys_msg_parm_vstr (msg_parms[1], img_out.tnam);
  sys_error_print (stat, 'img', 'close', msg_parms, 1);
  end.
