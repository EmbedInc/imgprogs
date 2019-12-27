{   Program IMAGE_FILTER <input file name> <output file name> [<options> . . .]
*
*   Read the input image file and create the output image file.  The image will be
*   shrunk and filtered in the process.  The legal command line options are:
*
*   -SHRINK n
*
*     Shrink the image by integer factor N in both dimensions.  The default for N
*     is DEFAULT_SHRINK.
*
*   -DSHRINK nx ny
*
*     Shrink the image with different shrink factors for the X and Y dimensions.  NX
*     and NY must be positive integers.  The default is to shrink the image by
*     DEFAULT_SHRINK in both dimensions.
*
*   -FILTER filter_file_name
*
*     Load the filter kernel funtion from a file.  The filter kernel function is always
*     assumed to be radially symmetric.  The filter file is an ASCII file that
*     describes the filter function value as a function of the radius from 0 to some
*     maximum value.  The first (non-comment) line in the filter file contains one
*     value indicating the maximum radius the filter function should be evaluated
*     at.  This is a floating point number in terms of output image pixel sizes.
*     Therefore, a value of 1.5 would cause a filter kernel that is 3x3 output pixels
*     in size.  The remaining lines in the file each contain a pair of numbers.  The
*     first number is a radius, and the second is the function value at that radius.
*     The function will be linearly interpolated between adjacent points.  The
*     radius values MUST be in ascending order.  Blank lines, and lines that start with
*     a star (*) are comment lines.  The default is a cos**2 filter with a radius of
*     DEFAULT_FILT_R output pixel widths.
}
program image_filter;
%include 'sys.ins.pas';
%include 'util.ins.pas';
%include 'string.ins.pas';
%include 'file.ins.pas';
%include 'img.ins.pas';
%include 'math.ins.pas';

const
  default_filt_r = 1.25;               {outer radius of default filter function}
  default_shrink = 2;                  {default input to output image shrink factor}
  max_img_width = 8192;                {max number of pixels accross input image}
  max_filt_width = 100;                {max subpixels accross filter kernel}
  max_filt_height = 100;               {max subpixels vertically in filter kernel}
  max_func_size = 1024;                {max number of samples allowed in filter func}
  max_lines_buffered = 5;              {max output image lines kept in memory buffer}
  default_func_samples = 100;          {number of samples in default filter function}
  integrate_grid_dim = 4;              {number of samples used in each dimension to
                                        integrate profile function over one filter
                                        kernel coeficient}

  max_img_x = max_img_width-1;         {max input image X pixel coordinate}
  max_filt_x = max_filt_width-1;       {max filter kernel X subpixel coordinate}
  max_filt_y = max_filt_height-1;      {max filter kernel Y subpixel coordinate}
  max_msg_parms = 4;                   {max parameters we can pass to a message}

type
  func_entry_t = record                {one entry in filter cross section array}
    r: real;                           {radius in output pixel widths}
    f: real;                           {function value at that radius}
    end;

  real_pixel_t = record                {a pixel with floating point intensities}
    red: real;                         {0.0 to 1.0 color intensities}
    grn: real;
    blu: real;
    alpha: real;                       {0.0 to 1.0 opaqueness fraction}
    wat: real;                         {total filter contribution weighting factor}
    end;

  real_pixel_p_t =                     {pointer to floating point pixel}
    ^real_pixel_t;

var
  in_fnam,                             {input image file name}
  out_fnam,                            {output image file name}
  filt_fnam:                           {filter function file name}
    %include '(cog)lib/string_treename.ins.pas';
  img_in: img_conn_t;                  {handle to open input image}
  img_out: img_conn_t;                 {handle to open output image}
  out_fmt:                             {format string for output image}
    %include '(cog)lib/string80.ins.pas';
  filt_id: file_conn_t;                {filter function file stream ID}
  stat: sys_err_t;                     {completion status code}
  func:                                {filter function cross section table}
    array[1..max_func_size] of func_entry_t;
  func_r: real;                        {max radius beyond with filter is all zero}
  func_n: sys_int_machine_t;           {number of data points in FUNC}
  default_filter: boolean;             {TRUE if internal default filter used}
  shrink_x, shrink_y: sys_int_machine_t; {image shrink factors for each dimension}
  filt_nx, filt_ny: sys_int_machine_t; {subpixel across filter kernel in X and Y}
  filt_outx, filt_outy: sys_int_machine_t; {X and Y output pixel size of filter kernel}
  filt:                                {convolution kernel for filter}
    array[0..max_filt_y, 0..max_filt_x] of real;
  fx, fy: sys_int_machine_t;           {FILT array subscript indicies}
  sym_fx, sym_fy: sys_int_machine_t;   {FILT array coordinates symmetric to FX,FY}
  r32: real;                           {scratch 32 bit floating point number}
  i, j: sys_int_machine_t;             {scratch integers and loop counters}
  iline:                               {current input image scan line}
    array[0..max_img_x] of img_pixel1_t;
  inpix: real_pixel_t;                 {current floating point input pixel}
  sfy, sfx: sys_int_machine_t;         {starting FILT coor for current input pixel}
  ix: sys_int_machine_t;               {X pixel index into current ILINE scan line}
  iy: sys_int_machine_t;               {Y coordinate of current scan line in ILINE}
  oline:                               {set of current output image scan lines}
    array[1..max_lines_buffered, 0..max_img_x] of real_pixel_t;
  olinep:                              {pointers to current active output lines}
    array[1..max_lines_buffered] of real_pixel_p_t;
  opixp:                               {pointers to current pixels in output lines}
    array[1..max_lines_buffered] of real_pixel_p_t;
  top_oy: sys_int_machine_t;           {top scan line number currently in OLINE}
  sox: sys_int_machine_t;              {output pixel X at left edge of filter kernel}
  ox: sys_int_machine_t;               {X pixel index into an output line in OLINE}
  oy: sys_int_machine_t;               {Y coor of a scan line in OLINE}
  pixp: real_pixel_p_t;                {scratch pointer to a pixel}
  done: boolean;                       {TRUE when done last output line}
  out_buf:                             {raw output image scan line}
    array[0..max_img_x] of img_pixel1_t;

  opt,                                 {command line option name}
  s:                                   {scratch string}
    %include '(cog)lib/string_treename.ins.pas';
  pick: sys_int_machine_t;             {number of token picked from list}
  msg_parm:                            {message parameter references}
    array[1..max_msg_parms] of sys_parm_msg_t;

label
  next_opt, done_opts, next_input_line, next_output_line, next_output_pixel,
  done_input_lines, done_output_lines;
{
***************************************************************************************
*
*   Internal subroutine READIN_FILTER_FUNC
*
*   Read the information from the filter profile input file into array FUNC.
*   The file is assumed to be already open on stream FILT_ID.
}
procedure readin_filter_func;

var
  buf: string_var256_t;                {one line input buffer}
  first: boolean;                      {TRUE if processing first data line in file}
  p: string_index_t;                   {input buffer parse pointer}
  stat: sys_err_t;                     {completion status code}

label
  next_line, fmt_error, error, eof;

begin
  buf.max := sizeof(buf.str);          {init max length of var string}
  func_n := 0;                         {init number of data points in function}
  first := true;                       {init to processing first data line in file}

next_line:                             {back here each new text line to read}
  file_read_text (filt_id, buf, stat); {read next line from input file}
  if file_eof(stat) then goto eof;     {hit end of file ?}
  if sys_error_check (stat, 'img', 'image_filter_read_filt_file', nil, 0)
    then goto error;
  if buf.len <= 0 then goto next_line; {ignore blank lines}
  if buf.str[1] = '*' then goto next_line; {comment line ?}
  p := 1;                              {init input line parse pointer}
  if first                             {check for first or subsequent data lines}

    then begin                         {this is the first data line in the file}
      string_token_fpm (buf, p, func_r, stat); {get floating point token value}
      if sys_error(stat) then goto fmt_error;
      first := false;                  {no longer processing first data line in file}
      end                              {done processing first data line in file}

    else begin                         {this is not first data line in file}
      func_n := func_n+1;              {one more entry in function profile table}
      if func_n > max_func_size then begin
        writeln ('Function table overflow.');
        goto error;
        end;
      string_token_fpm (buf, p, func[func_n].r, stat); {read radius token}
      if sys_error(stat) then goto fmt_error;
      string_token_fpm (buf, p, func[func_n].f, stat); {read function value}
      if sys_error(stat) then goto fmt_error;
      end                              {done processing non-first data line in file}
    ;                                  {done with first/non-first check}
  goto next_line;                      {back and process next line in file}

eof:                                   {jump here on end of filter file}
  file_close (filt_id);                {close filter input file}
  if func_n < 1 then begin
    writeln ('No data points found in function profile file.');
    sys_bomb;
    end;
  return;

fmt_error:                             {jump here on string conversion error}
  writeln ('Illegal floating point number.');
error:                                 {all errors while reading file come here}
  string_treename (filt_fnam, out_fnam); {make full tree name of input file}
  writeln ('Error occurred on line', filt_id.lnum:4, ' of file ', out_fnam.str:out_fnam.len);
  sys_bomb;
  end;
{
***************************************************************************************
*
*   Internal subroutine DEFAULT_FILTER_FUNC
*
*   Fill the filter function profile array (FUNC) with the default filter function.
*   The default filter COS(w)+1 evaluated for W from 0 to PI.  The overall magnitude
*   of the filter function is not important because this will be adjusted later.
}
procedure default_filter_func;

const
  da = 3.141593/(default_func_samples-1); {radians increment to next sample}
  dr = default_filt_r/(default_func_samples-1); {radius increment to next sample}

var
  a: real;                             {current angle in radians}
  r: real;                             {current radius}

begin
  a := 0.0;                            {init current angle for cosine}
  r := 0.0;                            {init current radius from center of function}
  func_r := default_filt_r;            {set max radius of filter function}
  for func_n := 1 to default_func_samples-1 do begin {once for each sample to fill in}
    func[func_n].r := r;               {set radius of this sample point}
    func[func_n].f := cos(a)+1.0;      {function value at this radius}
    a := a + da;                       {update angle to next sample point}
    r := r + dr;                       {update radius to next smaple point}
    end;                               {back and do next sample point}
  func[default_func_samples].r := default_filt_r; {fill in last point explicitly}
  func[default_func_samples].f := 0.0;
  func_n := default_func_samples;      {pass back the number of samples in FUNC}
  end;
{
***************************************************************************************
*
*   Internal function EVAL_KERNEL_ENTRY(SX,SY)
*
*   Evaluate the indicated filter kernel coeficient, given the filter profile table
*   in FUNC.  The filter kernel is an array of subpixels numbered 0,0 at the top left
*   corner.  The filter function will be sampled in a square grid inside the
*   subpixel at SX,SY.  These samples will be averaged to produce the coeficient value
*   for this subpixel.  The number of integration samples along each dimension
*   is set by INTEGRATE_GRID_DIM.
}
function eval_kernel_entry (
  in      sx: sys_int_machine_t;       {X subpixel coordinate within filter kernel}
  in      sy: sys_int_machine_t)       {Y subpixel coordinate within filter kernel}
  :real;

var
  cent_x, cent_y: real;                {coordinate of kernel center}
  rmult_x, rmult_y: real;              {mult factors to scale to function radius}
  dx, dy: real;                        {X and Y deltas to next integration sample}
  x, y: real;                          {current integration sample coordinates}
  start_x: real;                       {X coordinate of first sample in row}
  i, j: sys_int_machine_t;             {loop counters}
  acc: real;                           {integration accumulator}
  r: real;                             {filter profile function radius}

begin
  rmult_x := 1.0/shrink_x;             {use to scale subpix dist to out pix distance}
  rmult_y := 1.0/shrink_y;
  cent_x := rmult_x*filt_nx/2.0;       {make subpix coor of function center}
  cent_y := rmult_y*filt_ny/2.0;
  dx := rmult_x/integrate_grid_dim;    {make coor deltas to next integration point}
  dy := rmult_y/integrate_grid_dim;
  start_x := sx*rmult_x + 0.5*dx;      {X coor of left most column of samples}
  y := sy*rmult_y + 0.5*dy;            {Y coor of top row of samples}
  acc := 0.0;                          {init integration accumulator}

  for i := 1 to integrate_grid_dim do begin {down the rows of samples}
    x := start_x;                      {set X coor to first sample in row}
    for j := 1 to integrate_grid_dim do begin {across this row of samples}
      r := sqrt(sqr(x-cent_x)+sqr(y-cent_y)); {make radius at this sample point}
      acc := acc + math_ipolate(func, func_n, r); {find filter value at this radius}
      x := x+dx;                       {advance to next sample across}
      end;                             {back and do next sample point across row}
    y := y+dy;                         {advance to next row of samples down}
    end;                               {back and do next row of samples down}
  eval_kernel_entry :=                 {pass back average of all the sample points}
    acc/sqr(integrate_grid_dim);
  end;
{
***************************************************************************************
*
*   Internal subroutine WRITE_OUTPUT_LINE (PP,Y,DONE)
*
*   Write a scan line to the output image file.  PP is a pointer to the first pixel in
*   the scan line.  The scan line at PP is in floating point RGB format.  Y is the
*   output image file scan line number of the line to write.  Do not write the scan
*   line if it is outside the bounds of the output image.  DONE will be set to
*   TRUE after the last scan line is written.
}
procedure write_output_line (
  in      pp: real_pixel_p_t;          {pnt to first pixel in floating point scan line}
  in      y: sys_int_machine_t;        {output image Y coordinate of this scan line}
  out     done: boolean);              {TRUE if all done with last output line}

var
  p: real_pixel_p_t;                   {pointer to current pixel}
  x: sys_int_machine_t;                {X coordinate of current pixel}
  r: real;                             {clipped color value}

begin
  done := false;                       {init to not done with last scan line}
  if y < 0 then return;                {not a valid scan line, nothing to do ?}
  if y < img_out.y_size then begin     {scan line is within range ?}
    p := pp;                           {make local copy of pixel pointer}
    for x := 0 to img_out.x_size-1 do begin {once for each pixel in scan line}
      if p^.wat < 0.999 then begin     {adjust for not unity weight factor ?}
        r := 1.0/p^.wat;               {make scale factor to adjust colors by}
        p^.red := p^.red*r;            {normalize the color values}
        p^.grn := p^.grn*r;
        p^.blu := p^.blu*r;
        p^.alpha := p^.alpha*r;
        end;
      r := p^.red;                     {convert and copy red value}
      if r < 0.0 then r := 0.0;        {clip at black}
      if r > 255.0 then r := 255.0;    {clip at white}
      out_buf[x].red := trunc(r+0.5);  {convert to integer and stuff into output buf}
      r := p^.grn;                     {convert and copy green value}
      if r < 0.0 then r := 0.0;
      if r > 255.0 then r := 255.0;
      out_buf[x].grn := trunc(r+0.5);
      r := p^.blu;                     {convert and copy blue value}
      if r < 0.0 then r := 0.0;
      if r > 255.0 then r := 255.0;
      out_buf[x].blu := trunc(r+0.5);
      r := p^.alpha;                   {convert and copy alpha value}
      if r < 0.0 then r := 0.0;
      if r > 255.0 then r := 255.0;
      out_buf[x].alpha := trunc(r+0.5);
      p :=                             {advance pointer to next floating point pixel}
        real_pixel_p_t(integer32(p)+sizeof(p^));
      end;                             {back and copy next pixel}
    img_write_scan1 (img_out, out_buf, stat); {write scan line to output file}
    sys_error_abort (stat, 'img', 'write_scan_line', nil, 0);
    end;                               {done handling scan line within range}
  done := y >= img_out.y_size - 1;     {past or at end of output file ?}
  end;
{
***************************************************************************************
*
*   Start of main routine.
}
begin
  string_cmline_init;                  {init for processing command line}

  string_cmline_token (in_fnam, stat); {get input file name from command line}
  string_cmline_req_check (stat);

  string_cmline_token (out_fnam, stat); {get output file name from command line}
  string_cmline_req_check (stat);

  img_open_read_img (in_fnam, img_in, stat); {open input image for read}
  sys_msg_parm_vstr (msg_parm[1], in_fnam);
  sys_error_abort (stat, 'img', 'open_read', msg_parm, 1);

  if img_in.x_size > max_img_width then begin
    sys_message_bomb ('img', 'image_too_wide', nil, 0);
    end;
{
*   The two required command line parameters (input and output image file names) have
*   been read in and the input image file has been opened.  Now do any initialization
*   necessary before processing the command line options.
}
  shrink_x := default_shrink;          {set shrink factor to default}
  shrink_y := default_shrink;
  func_n := 0;                         {indicate no filter file read in so far}
  default_filter := true;              {init to default filter function in use}
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
    '-SHRINK -FILTER -DSHRINK',        {valid command line option names}
    pick);                             {number of picked option}
  case pick of                         {do routine for specific option}
{
*   Command line option: -SHRINK n
}
1: begin
  string_cmline_token_int (shrink_x, stat);
  string_cmline_parm_check (stat, opt); {check for parameter error}
  if shrink_x < 1 then begin
    sys_message_bomb ('img', 'image_filter_bad_shrink', nil, 0);
    end;
  shrink_y := shrink_x;
  end;
{
*   Command line option: -FILTER <file name>
}
2: begin
  string_cmline_token (filt_fnam, stat); {get filter file name}
  string_cmline_parm_check (stat, opt); {check for parameter error}
  file_open_read_text (filt_fnam, '', filt_id, stat); {open ASCII filter input file}
  sys_msg_parm_vstr (msg_parm[1], filt_fnam);
  sys_error_abort (stat, 'file', 'open_input_read_text', msg_parm, 1);
  readin_filter_func;                  {read file info into FUNC array}
  default_filter := false;             {indicate we are using external filter}
  end;
{
*   Command line option: -DSHRINK nx ny
}
3: begin
  string_cmline_token_int (shrink_x, stat);
  string_cmline_parm_check (stat, opt);
  string_cmline_token_int (shrink_y, stat);
  string_cmline_parm_check (stat, opt);
  if (shrink_x < 1) or (shrink_y < 1) then begin
    sys_message_bomb ('img', 'image_filter_bad_shrink', nil, 0);
    end;
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
*   Done processing all the command line options.
*
*   Now determine the size of the convolusion filter kernel in various forms.  FILT_NX
*   FILT_NY are the number of subpixels (input image pixels) across the X and Y
*   dimension of the kernel.  FILT_OUTX and FILT_OUTY is the same, only for the output
*   pixels.
}
  if func_n <= 0 then begin            {no filter function read from external file ?}
    default_filter_func;               {fill FUNC with default filter profile}
    end;
  if (shrink_x & 1) = 0                {check for even/odd shrink factor}
    then filt_nx := round((shrink_x*func_r)+0.4999)*2 {subpixel corner in center}
    else filt_nx := round(shrink_x*func_r)*2+1; {subpixel exactly in center}
  if (shrink_y & 1) = 0                {check for even/odd shrink factor}
    then filt_ny := round((shrink_y*func_r)+0.4999)*2 {subpixel corner in center}
    else filt_ny := round(shrink_y*func_r)*2+1; {subpixel exactly in center}
  filt_outx := (filt_nx div shrink_x) ! 1; {make X kernel size in output pixels}
  if filt_outx*shrink_x < filt_nx
    then filt_outx := filt_outx+2;
  filt_outy := (filt_ny div shrink_y) ! 1; {make Y kernel size in output pixels}
  if filt_outy*shrink_y < filt_ny
    then filt_outy := filt_outy+2;
  if filt_outy > max_lines_buffered then begin
    writeln ('Filter kernel is too many output pixels tall.');
    sys_bomb;
    end;
  filt_nx := filt_outx*shrink_x;       {make sure kernel covers WHOLE output pixels}
  filt_ny := filt_outy*shrink_y;
  if filt_nx > max_filt_width then begin
    writeln ('Too many subpixels in filter kernel horizontally.  Decrease shrink');
    writeln ('factor or filter function radius, or increase MAX_FILT_WIDTH in source');
    writeln ('code.');
    sys_bomb;
    end;
  if filt_ny > max_filt_width then begin
    writeln ('Too many subpixels in filter kernel vertically.  Decrease shrink');
    writeln ('factor or filter function radius, or increase MAX_FILT_HEIGHT in');
    writeln ('source code.');
    sys_bomb;
    end;
{
*   The size of the filter kernel has been set.  Now fill in the FILT array.  This
*   array contains the actual convolution coeficients, and will be normalized so that
*   the total volume adds up to 1.  FILT[y,x] is organized as a 2 dimensional array
*   of subpixels.  The array is centered on the center of an output pixel,
*   and the array elements indicate the contribution for each subpixel into the output
*   pixel.  The top left corner of the kernel is at index 0,0.  Only one quadrant of
*   subpixels will be evaluated, and the results copied to the other quadrants.
*   (Yes, I know that the kernel really has 8-way symmetry, but that gets to be a
*   real pain to deal with when the kernel is not square, meaning the shrink factors
*   are not equal).
*
*   FILT will be set up in three passes.  In the first pass, a quadrant of the kernel
*   is scanned.  Values are evaluated and immediately stuffed into all four quadrants.
*   The second pass sums up the total value of all the coeficients.  This is not done
*   in pass one, since the quadrants may overlap for some kernel dimensions.  In pass
*   three, the result of pass two is used to normalize the whole kernel volume to one.
*   This insures that the average image intensity is not altered by the filtering
*   process.
}
  for fy := 0 to (filt_ny-1) div 2 do begin {scan from top to half way down}
    sym_fy := filt_ny-1-fy;            {make symmetric Y coordinate to FY}
    for fx := 0 to (filt_nx-1) div 2 do begin {scan left to half way across}
      sym_fx := filt_nx-1-fx;          {make symmetric X coordinate to FX}
      filt[fy, fx] := eval_kernel_entry(fx, fy); {fill in this quadrant directly}
      filt[fy, sym_fx] := filt[fy, fx]; {copy value to remaining 3 quardants}
      filt[sym_fy, fx] := filt[fy, fx];
      filt[sym_fy, sym_fx] := filt[fy, fx];
      end;                             {back for next subpixel accross this line}
    end;                               {back for next line down the filter kernel}

  r32 := 0.0;                          {init filter volume accumulator}
  for fy := 0 to filt_ny-1 do begin    {down the scan lines in the kernel}
    for fx := 0 to filt_nx-1 do begin  {across this kernel scan line}
      r32 := r32+filt[fy, fx];         {accumulate this entry in total sum}
      end;                             {back for next entry across scan line}
    end;                               {back for next scan line down in kernel}

  if abs(r32) < 1.0E-6 then begin
    writeln ('Total filter volume is too small to normalize to unity.');
    sys_bomb;
    end;
  r32 := 1.0/r32;                      {factor to scale kernel to unit volume}
  for fy := 0 to filt_ny-1 do begin    {down the scan lines in the kernel}
    for fx := 0 to filt_nx-1 do begin  {across this kernel scan line}
      filt[fy, fx] := filt[fy, fx]*r32; {scale this filter kernel entry}
      end;                             {back for next entry across scan line}
    end;                               {back for next scan line down in kernel}
{
*   The filter kernel is all set up.  Now open the output image file.
}
  string_list_pos_last (img_in.comm);  {move to last comment line}
  string_list_line_add (img_in.comm);  {create comment line for date}
  sys_date_time1 (img_in.comm.str_p^); {init our comment line with date/time string}
  string_appends (img_in.comm.str_p^, '  Filtered with program IMAGE_FILTER.');

  string_list_line_add (img_in.comm);  {create comment line for filter parameters}
  string_appends (img_in.comm.str_p^, '  Shrunk');
  string_append1 (img_in.comm.str_p^, ' ');
  string_f_int (s, shrink_x);
  string_append (img_in.comm.str_p^, s);
  string_append1 (img_in.comm.str_p^, 'x');
  string_f_int (s, shrink_y);
  string_append (img_in.comm.str_p^, s);
  if default_filter                    {check where the filter function came from}
    then begin                         {filter came from internal default routine}
      string_appends (img_in.comm.str_p^, ' using the default filter.');
      end
    else begin                         {filter function came from external file}
      string_appends (img_in.comm.str_p^, ' using filter from file');
      string_append1 (img_in.comm.str_p^, ' ');
      string_treename (filt_fnam, s);
      string_append (img_in.comm.str_p^, s);
      string_append1 (img_in.comm.str_p^, '.');
      end
    ;
  string_appends (img_in.comm.str_p^, '  Source image file:');

  string_list_line_add (img_in.comm);  {create comment line for file name}
  string_appendn (img_in.comm.str_p^, '  ', 2);
  string_append (img_in.comm.str_p^, img_in.tnam);

  out_fmt.len := 0;                    {init output image format string}
  string_appends (out_fmt, 'RED 8 GREEN 8 BLUE 8 ALPHA');
  string_append1 (out_fmt, ' ');
  if img_in.bits_alpha > 0
    then begin                         {input image DOES have alpha values}
      string_appendn (out_fmt, '8', 1);
      end
    else begin                         {input image does NOT have alpha values}
      string_appendn (out_fmt, '0', 1);
      end
    ;

  ix := img_in.x_size div shrink_x;    {make output file resolution}
  iy := img_in.y_size div shrink_y;
  r32 := img_in.aspect *               {output file aspect ratio}
    (ix * img_in.y_size) / (iy * img_in.x_size);

  img_open_write_img (                 {open output image file}
    out_fnam,                          {file name}
    r32,                               {aspect ratio of correctly viewed image}
    ix, iy,                            {image size in pixels}
    '',                                {select default image file type}
    out_fmt,                           {file format string}
    img_in.comm,                       {comments list handle}
    img_out,                           {returned image connection handle}
    stat);
  sys_error_abort (stat, 'img', 'open_write', nil, 0);
{
*   Output image file is open.  The input image file was opened earlier.
*   Now filter the input image into the output image.  There are FILT_OUTY number of
*   output image scan lines buffered in memory.  They corresponds to the output image
*   height of the filter kernel.  One input image scan line is read at a time.  The
*   contribution from each input pixel is accumulated in all the output image pixels
*   that it can effect.  The input scan line is in ILINE, and its scan line number
*   is in IY.  The output scan lines are in OLINE, and are pointed to by the
*   first FILT_OUTY entries of OLINEP.  OPIXP points to the left most pixel in each
*   scan line effected by the current input pixel.  TOP_OY gives the scan line number
*   of the top output scan line currently in OLINE.  OLINE contains floating point
*   pixel values.  Once all the accumulation is finished, these are converted to
*   integer pixel values in OUT_BUF and written to the output image file.
*
*   Start by initializing the data structures before looping thru the input scan lines.
}
  top_oy := -((filt_outy-1) div 2);    {scan line number of first buffered out line}
  for i := 1 to filt_outy do begin     {once for each buffered output line}
    olinep[i] := addr(oline[i, 0]);    {make pointer to this scan line}
    oy := top_oy+i-1;                  {make Y coordinate of this out image scan line}
    if (oy >= 0) and (oy < img_out.y_size) then begin {scan line really exists ?}
      pixp := olinep[i];               {make pointer to first pixel in scan line}
      for j := 1 to img_out.x_size do begin {once for each pixel in scan line}
        pixp^.red := 0.0;              {init accumulated values for this pixel}
        pixp^.grn := 0.0;
        pixp^.blu := 0.0;
        pixp^.alpha := 0.0;
        pixp^.wat := 0.0;              {init accumulated weight for this pixel}
        pixp := real_pixel_p_t(        {advance pointer to next pixel}
          integer32(pixp)+sizeof(pixp^));
        end;                           {back and init next pixel}
      end;                             {done handling scan line that exists}
    end;                               {back and init next buffered output scan line}
  iy := -1;                            {init number of current input scan line}
{
*   Main loop.  Come back here each new output image scan line.
}
next_output_line:
  for sfy := 0 to shrink_y-1 do begin  {once for each input line per output line}
    iy := iy+1;                        {make Y coordinate of next input scan line}
    if iy >= img_in.y_size             {already read whole input image ?}
      then goto done_input_lines;
    img_read_scan1 (img_in, iline, stat); {read next input image scan line}
    sys_error_abort (stat, 'img', 'read_scan_line', nil, 0);
    ix := 0;                           {init current input pixel X coordinate}
    sox := -((filt_outx-1) div 2);     {out pixel X at left edge of filter kernel}
    for i := 1 to filt_outy do begin   {once for each buffered output line}
      opixp[i] := real_pixel_p_t(      {reset curr out pix pointers to start of line}
        integer32(olinep[i])-sizeof(real_pixel_t)*(filt_outx div 2));
      end;
next_output_pixel:                     {back here to advance to next output pixel}
    for sfx := 0 to shrink_x-1 do begin {once for each X input pixel per output pixel}
      inpix.red := iline[ix].red;      {convert input pixel to floating point}
      inpix.grn := iline[ix].grn;
      inpix.blu := iline[ix].blu;
      inpix.alpha := iline[ix].alpha;
{
*   The floating point representation of the current input pixel is in INPIX.  SFX and
*   SFY are the FILT subscripts for the appropriate subpixel in the top left output
*   pixel of the filter kernel.  SOX is the output pixel X coordinate corresponding
*   to the left edge of the filter kernel, and TOP_OY is the output pixel Y coordinate
*   corresponding the to top edge of the filter kernel.  Now accumulate the input
*   pixel contribution in all the output pixels covered by the current filter kernel
*   position.
}
      oy := top_oy-1;                  {init output image Y coor of current scan line}
      fy := filt_ny+sfy;               {init current filter kernel Y}
      for i := 1 to filt_outy do begin {down output scan lines in the filter kernel}
        fy := fy-shrink_y;             {make FILT Y index this new line}
        oy := oy+1;                    {make current output image Y coordinate}
        if (oy < 0) or (oy >= img_out.y_size) {Y coordinate not in image ?}
          then next;                   {if so, skip this scan line}
        ox := sox-1;                   {init output image X coordinate}
        fx := filt_nx+sfx;             {init current filter kernel X}
        pixp := real_pixel_p_t(        {init out pix pointer to this scan line}
          integer32(opixp[i])-sizeof(pixp^));
        for j := 1 to filt_outx do begin {across output pixels in filter kernel}
          fx := fx-shrink_x;           {advance kernel index to next output pixel}
          ox := ox+1;                  {make current output image X coordinate}
          pixp := real_pixel_p_t(      {advance pointer to this output pixel}
            integer32(pixp)+sizeof(pixp^));
          if (ox < 0) or (ox >= img_out.x_size) {X coordinate not in image ?}
            then next;                 {if so, skip this pixel}
          pixp^.red := pixp^.red +     {accumulate pixel contributions}
            inpix.red*filt[fy, fx];
          pixp^.grn := pixp^.grn +
            inpix.grn*filt[fy, fx];
          pixp^.blu := pixp^.blu +
            inpix.blu*filt[fy, fx];
          pixp^.alpha := pixp^.alpha +
            inpix.alpha*filt[fy, fx];
          pixp^.wat := pixp^.wat +     {accumulate weight of this output pixel}
            filt[fy, fx];
          end;                         {back and accumulate into next output pixel}
        end;                           {back and do next kernel line down}
{
*   Done with this input image pixel.
}
      ix := ix+1;                      {advance to next input image pixel}
      if ix >= img_in.x_size           {done with this input scan line ?}
        then goto next_input_line;     {back and process next input scan line}
      end;                             {back for next pixel at same kernel position}
{
*   Done with a row of SHRINK_X input pixels.  Now move the filter kernel over one
*   output pixel before processing the next set of input pixels.
}
    sox := sox+1;                      {filter kernel now starts one output pixel over}
    for i := 1 to filt_outy do begin   {once for each output line in the buffers}
      opixp[i] := real_pixel_p_t(      {advance output pix pntrs to new kernel start}
        integer32(opixp[i])+sizeof(pixp^));
      end;
    goto next_output_pixel;            {back and do next set of input pixels}
{
*   Done with the current input scan line.
}
next_input_line:                       {jump here to advance to next input scan line}
    end;                               {back for next input line in same output line}
{
*   Done with a set of SHRINK_Y input scan lines.  Now move the filter kernel down
*   one output scan line before the next set of input scan lines.  This includes
*   rolling the buffer of output scan lines, and possibly writing a finished output
*   scan line to the output file.
}
  write_output_line (olinep[1], top_oy, done); {write output scan line if within image}
  if done then goto done_output_lines; {all done writing output image file}
  pixp := olinep[1];                   {save pointer to scan line rolled away}
  for i := 1 to filt_outy-1 do begin   {once for each scan line in rolling buffer}
    olinep[i] := olinep[i+1];          {move each line one further to the top}
    end;
  olinep[filt_outy] := pixp;           {rolled-out buffer re-used for new scan line}
  top_oy := top_oy+1;                  {top line in buffer is now one scan line lower}
  i := top_oy+filt_outy-1;             {output Y coordinate of newly created scan line}
  if (i >= 0) and (i < img_out.y_size) then begin {new line exists ?}
    for i := 1 to img_out.x_size do begin {once for each pixel in scan line}
      pixp^.red := 0.0;                {init accumulated values for this pixel}
      pixp^.grn := 0.0;
      pixp^.blu := 0.0;
      pixp^.alpha := 0.0;
      pixp^.wat := 0.0;
      pixp := real_pixel_p_t(          {advance pointer to next pixel}
        integer32(pixp)+sizeof(pixp^));
      end;                             {back and init next pixel}
    end;                               {done clearing new scan line if inside image}
  goto next_output_line;               {back with filter kernel moved down one line}
{
*   All the input scan lines have been processed.  Write out any output image scan
*   lines that may still be in the rolling buffer.  Subroutine WRITE_OUTPUT_LINE
*   is called for each scan line still in the rolling buffers.
}
done_input_lines:
  for i := 1 to filt_outy do begin     {once for each scan line in rolling buffers}
    write_output_line (olinep[i], top_oy+i-1, done); {write this line to output file}
    if done then goto done_output_lines; {all done writing output image file}
    end;
{
*   The whole input file has been read, and all the remaining output scan lines in
*   the rolling buffers have been written to the output file, but the last output file
*   scan line has still not been written.  This is an error.
}
  writeln ('Wrote last scan line in buffer, but output file still not full.');
  sys_bomb;                            {exit with "bad output" severity}

done_output_lines:                     {jump here if output file all written normally}
  img_close (img_in, stat);            {close input image file}
  sys_error_print (stat, 'img', 'close', nil, 0);
  img_close (img_out, stat);           {close output image file}
  sys_error_print (stat, 'img', 'close', nil, 0);
  end.
