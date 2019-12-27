{   Program IMAGE_RESIZE [options]
*
*   Copy an image from one image file to another, and change the number of
*   pixels in the image.  The command line options are:
*
*   <input file name>
*
*   -IN <input file name>
*
*   <output file name>
*
*   -OUT <output file name>
*
*   -SIZE nx ny
*
*   -SCALE s, -SHRINK s
*
*   -FIT nx ny
*
*   -PASP asp
*
*   -NFILT, -FILT
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
  default_width_k = 640;               {max width if no size/scale specified}
  default_height_k = 512;              {max height if no size/scale specified}
  filt_index_n_k = 1000;               {num filter steps from 0 to max radius}
  filt_val_n_k = 1000;                 {number of possible filter coeficient values}
  max_scans_k = 65536;                 {max scan lines we can handle in input image}
  max_msg_args = 2;                    {max arguments we can pass to a message}

  filt_index_max_k =                   {max filter index value}
    filt_index_n_k - 1;
  filt_val_max_k =                     {max possible filter coeficient value}
    filt_val_n_k - 1;
  max_in_scan_k = max_scans_k - 1;     {largest allowable input image scan line num}

var
  fnam_in, fnam_out:                   {input and output file names}
    %include '(cog)lib/string_treename.ins.pas';
  form: string_var8192_t;              {user output image format string}
  ftype: string_var32_t;               {output image file type name}
  iname_set: boolean;                  {TRUE if the input file name already set}
  oname_set: boolean;                  {TRUE if the output file name already set}
  scale_set: boolean;                  {scale factors explicitly set on TRUE}
  filter: boolean;                     {TRUE if filtering instead of point sampling}
  scalex, scaley: real;                {horiz and vert image size scale factors}
  osizex, osizey: sys_int_machine_t;   {desired output image size in pixels}
  fitx, fity: sys_int_machine_t;       {image size to fit within}
  img_in, img_out: img_conn_t;         {connection handles to in and out images}
  comm: string_list_t;                 {image file comment lines}
  ucomm: string_list_t;                {explicit user comments}
  radx, rady: real;                    {filter radii in input pixels}
  radxr, radyr: real;                  {reciprocals of RADX and RADY}
  iradx, irady: sys_int_machine_t;     {max pixles need to search in each direction}
  i, j: sys_int_machine_t;             {scratch integers and loop counters}
  sz: sys_int_adr_t;                   {memory size}
  adr: sys_int_adr_t;                  {scratch address}
  ipaspect, opaspect: real;            {input and output image pixel aspec ratios}
  fp: real;                            {scratch floating point number}
  r2: real;                            {current radius squared}
  lx, rx: sys_int_machine_t;           {left/right input X of current filter window}
  ty, by: sys_int_machine_t;           {top/bottom input Y of current filter window}
  last_ix, last_iy: sys_int_machine_t; {last input pixel coor in each dimension}
  ix, iy: sys_int_machine_t;           {current input pixel coordinate}
  ox, oy: sys_int_machine_t;           {current output pixel coordinate}
  red, grn, blu, alpha: sys_int_conv32_t; {component values of curr output pixel}
  acc: sys_int_conv32_t;               {total filter weight accumulated so far}
  topy, leftx: real;                   {input coor of top/left output pixel centers}
  x, y: real;                          {input pixel coor of current output pixel}
  dx, dy: real;                        {output pixel size in input pixels}
  inbuf_p: img_scan1_arg_p_t;          {pointer to block of input scan lines}
  inbuf_n: sys_int_machine_t;          {number of scan lines in input buffer}
  oscan_p: img_scan1_arg_p_t;          {pointer to output scan line buffer}
  scan_p: img_scan1_arg_p_t;           {scratch pointer to an input scan line}
  iscanp:                              {pointers to each input image scan line}
    array[0..max_in_scan_k] of img_scan1_arg_p_t;

  opt:                                 {upcased command line option}
    %include '(cog)lib/string_treename.ins.pas';
  parm:                                {command line option parameter}
    %include '(cog)lib/string_treename.ins.pas';
  pick: sys_int_machine_t;             {number of token picked from list}
  msg_parm:                            {references arguments passed to a message}
    array[1..max_msg_args] of sys_parm_msg_t;
  stat: sys_err_t;                     {completion status code}

  filt: array [                        {R*R to convolution coeficient table}
    0 .. filt_index_max_k]             {normalized square of distance from center}
    of sys_int_conv16_t;               {0 to FILT_INDEX_MAX_K scale factor}

label
  next_opt, err_parm, parm_bad, done_opts, retry_scale, filtering, leave;
{
******************************************************************
*
*   Local subroutine MAKE_RADIUS (SCALE, RAD)
*
*   Calculate the convolution filter radius in RAD.  RAD will be in units
*   of input pixel widths.  SCALE is the output pixel image dimension in
*   pixels divided by the input image dimension in pixels.
}
procedure make_radius (                {calculate convolution filter radius}
  in      scale: real;                 {output pixel dimension / input pixel dim}
  out     rad: real);                  {filter radius in input pixel sizes}
  val_param;

const
  rad_shrink_k = 1.25;                 {filter radius for "large" shrink}
  lnh = ln(0.5);                       {log of scale where max radius starts}

var
  m1, m2: real;                        {linear blend factors}

begin
  if scale >= 1.0 then begin           {image is being enlarged, not shrunken ?}
    rad := 1.0;
    return;
    end;

  if scale <= 0.5 then begin           {shrinking by more than 2:1 ?}
    rad := rad_shrink_k / scale;
    return;
    end;
{
*   The image is being shrunk, but less than a factor of 2.
}
  m2 := ln(scale) / lnh;               {mult factor for "shrink" radius}
  m1 := 1.0 - m2;                      {mult factor for radius 1}
  rad :=                               {blend between radius 1 and shrink radius}
    m1 + m2 * rad_shrink_k;
  rad := rad / scale;                  {take pixel size difference into account}
  end;
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
  scale_set := false;                  {init to use output size instead of scale}
  scalex := 1.0;                       {init to benign scale factor}
  scaley := 1.0;
  osizex := 0;                         {init to default output width}
  osizey := 0;                         {init to default output height}
  opaspect := 1.0;                     {init output pixel aspect ratio}
  fitx := default_width_k;             {init image size to fit within}
  fity := default_height_k;
  filter := true;                      {init to filtering, not point sampling}
  string_list_init (ucomm, util_top_mem_context); {init explicit user comments list}
  ucomm.deallocable := false;
  form.len := 0;                       {init to no output format string supplied}
  ftype.len := 0;                      {init to use default output file type}
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
    '-IN -OUT -SIZE -SCALE -FIT -PASP -NFILT -FILT -SHRINK -COM -FTYPE -FORM',
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
*   -SIZE nx ny
}
3: begin
  string_cmline_token_int (osizex, stat);
  if sys_error(stat) then goto err_parm;
  if osizex < 0 then goto parm_bad;
  string_cmline_token_int (osizey, stat);
  if sys_error(stat) then goto err_parm;
  if osizey < 0 then goto parm_bad;
  scale_set := false;
  end;
{
*   -SCALE s
}
4: begin
  string_cmline_token_fpm (scalex, stat);
  if sys_error(stat) then goto err_parm;
  if scalex < 0.0 then goto parm_bad;
  scaley := scalex;
  scale_set := true;
  end;
{
*   -FIT nx ny
}
5: begin
  string_cmline_token_int (fitx, stat);
  if sys_error(stat) then goto err_parm;
  if fitx < 1 then goto parm_bad;
  string_cmline_token_int (fity, stat);
  if sys_error(stat) then goto err_parm;
  if fity < 1 then goto parm_bad;
  end;
{
*   -PASP asp
}
6: begin
  string_cmline_token_fpm (opaspect, stat);
  end;
{
*   -NFILT
}
7: begin
  filter := false;
  end;
{
*   -FILT
}
8: begin
  filter := true;
  end;
{
*   -SHRINK s
}
9: begin
  string_cmline_token_fpm (fp, stat);
  if sys_error(stat) then goto err_parm;
  if fp < 0.0001 then goto parm_bad;
  scalex := 1.0 / fp;
  scaley := scalex;
  scale_set := true;
  end;
{
*   -COM <comment>
}
10: begin
  string_cmline_token (parm, stat);
  if sys_error(stat) then goto err_parm;
  ucomm.size := parm.len;
  string_list_line_add (ucomm);        {create new user comments line}
  string_copy (parm, ucomm.str_p^);    {save user comment line}
  end;
{
*   -FTYPE filetype
}
11: begin
  string_cmline_token (ftype, stat);
  end;
{
*   -FORM <output format string>
}
12: begin
  string_cmline_token (parm, stat);
  if sys_error(stat) then goto err_parm;
  if form.len > 0 then begin           {there is a previous format string ?}
    string_append1 (form, ' ');        {add separator before new format string}
    end;
  string_append (form, parm);
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
  if img_in.y_size > max_scans_k then begin {too many scan lines in input image ?}
    sys_msg_parm_int (msg_parm[1], max_scans_k);
    sys_msg_parm_int (msg_parm[2], img_in.y_size);
    sys_message_bomb ('img', 'image_too_tall_n', msg_parm, 2);
    end;

  last_ix := img_in.x_size - 1;        {last input pixel coor in each dimension}
  last_iy := img_in.y_size - 1;
  ipaspect :=                          {input pixel aspect ratio}
    img_in.aspect * img_in.y_size / img_in.x_size;

retry_scale:                           {back here to recompute scaling}
  if scale_set
    then begin                         {scale factor was set explicitly}
      osizex := round(img_in.x_size * scalex); {make out image width in pixels}
      osizex := max(osizex, 1);
      osizey := round(img_in.y_size * scaley); {make out image height in pixels}
      osizey := max(osizey, 1);
      end
    else begin                         {scale set implicitly by output pixel size}
      if (osizex = 0) and (osizey = 0) then begin {out size completely default ?}
        fp :=                          {aspect ratio of full FIT rectangle}
          (fitx / fity) * opaspect;
        if fp <= img_in.aspect
          then begin                   {the tightest fit is horizontally}
            scalex := fitx / img_in.x_size;
            scaley := scalex * opaspect / ipaspect;
            end
          else begin                   {the tightest fit is veritically}
            scaley := fity / img_in.y_size;
            scalex := scaley * ipaspect / opaspect;
            end
          ;                            {done figuring out scale factors}
        scale_set := true;             {indicate to adjust to scale factors}
        goto retry_scale;              {back to recompute with new settings}
        end;                           {done handling totally default output size}
      if (osizex = 0) then begin       {use default width ?}
        scaley := osizey / img_in.y_size; {get scale factor from vertical sizes}
        scalex := scaley * ipaspect / opaspect;
        scale_set := true;             {indicate to adjust to scale factors}
        goto retry_scale;              {back to recompute with new settings}
        end;
      if (osizey = 0) then begin       {use default height ?}
        scalex := osizex / img_in.x_size; {get scale factor from horizontal sizes}
        scaley := scalex * opaspect / ipaspect;
        scale_set := true;             {indicate to adjust to scale factors}
        goto retry_scale;              {back to recompute with new settings}
        end;
      scalex := osizex / img_in.x_size; {make final pixel size scale factors}
      scaley := osizey / img_in.y_size;
      end                              {done handling scale set by sizes}
    ;

  dx := 1.0 / scalex;                  {make out pixel sizes in input pixel units}
  dy := 1.0 / scaley;
  opaspect :=                          {set final output pixel aspect ratio}
    ipaspect * scaley / scalex;
{
*   The final scale factors and output image size have been determined.
*   The output image will be OSIZEX x OSIZEY pixels in size, and the output to
*   input pixel size scale factors are SCALEX and SCALEY.
*
*   Now open the image output file.
}
  if not oname_set then begin          {use default output file name ?}
    string_copy (img_in.gnam, fnam_out); {use generic name of input image file}
    end;

  string_list_copy (                   {make local copy of input image comments}
    img_in.comm, comm, util_top_mem_context);
  string_list_pos_last (comm);         {go to last comment line in list}

  sys_date_time1 (opt);                {add our first comment line}
  string_appends (opt, '  Resized '(0));
  string_f_int (parm, img_in.x_size);
  string_append (opt, parm);
  string_append1 (opt, ',');
  string_f_int (parm, img_in.y_size);
  string_append (opt, parm);
  string_appends (opt, ' to '(0));
  string_f_int (parm, osizex);
  string_append (opt, parm);
  string_append1 (opt, ',');
  string_f_int (parm, osizey);
  string_append (opt, parm);
  string_appends (opt, ' pixels from file'(0));
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
  string_appends (opt, ' by IMAGE_RESIZE'(0));
  if abs(opaspect - 1.0) > 0.0001 then begin {not default pixel aspect ratio ?}
    string_appends (opt, ' -PASP '(0));
    string_f_fp_fixed (parm, opaspect, 3);
    string_append (opt, parm);
    end;
  if not filter then begin             {non-default filter ?}
    string_appends (opt, ' -NFILT'(0));
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
    opaspect * osizex / osizey,        {whole image aspect ratio}
    osizex, osizey,                    {image size in pixels}
    ftype.str,                         {driver name or will default from FNAM_OUT}
    opt,                               {driver format string}
    comm,                              {comment lines descriptor}
    img_out,                           {returned connection handle to new image}
    stat);
  sys_msg_parm_vstr (msg_parm[1], fnam_out);
  sys_error_abort (stat, 'img', 'open_write', msg_parm, 1);
{
*   Set up the convolution coeficients table FILT.  This table provides the
*   0 to FILT_VAL_MAX_K relative input pixel weight, given the normalized square
*   of its distance from the center of the output pixel.  The distance squared
*   is normalized to the 0 to FILT_INDEX_N_K range.  All values beyond
*   FILT_INDEX_MAX_K are zero.
}
  if filter then begin                 {smooth filtering is enabled ?}
    for i := 0 to filt_index_max_k do begin {once for each coeficient table entry}
      fp := (i + 0.5) / filt_index_n_k; {make 0.0 to 1.0 radius squared}
      fp := sqrt(fp);                  {make 0.0 to 1.0 radius}
      fp := (cos(fp*3.141593) + 1) / 2.0; {make 0.0 to 1.0 coeficient value}
      j := trunc(fp * filt_val_n_k);   {make raw coeficient value}
      j := min(j, filt_val_max_k);     {clip to maximum allowed range}
      filt[i] := j;                    {set this coeficient value}
      end;
    end;

  make_radius (scalex, radx);          {calculate horizontal filter radius}
  radxr := 1.0 / radx;                 {save radius reciprocal}
  iradx := trunc(radx + 0.5);          {max pixels need to scan left/right}

  make_radius (scaley, rady);          {calculate vertical filter radius}
  radyr := 1.0 / rady;                 {save radius reciprocal}
  irady := trunc(rady + 0.5);          {max pixels need to scan top/bottom}
{
*   Set up the input and output buffers.
}
  if filter
    then begin                         {convolution filtering enabled}
      inbuf_n := (irady * 2) + 1;      {max in lines we need to buffer}
      end
    else begin                         {point sampling, no convolutions}
      inbuf_n := 1;
      end
    ;
  sz := sizeof(img_pixel1_t) * img_in.x_size; {size of each input scan line}
  img_mem_alloc (img_in, sz * inbuf_n, inbuf_p); {alloc mem for input scan line bufs}

  j := 0;                              {init buffer index for next scan line}
  for i := 0 to last_iy do begin       {once for each input scan line pointer}
    adr := sys_int_adr_t(inbuf_p) +    {address of first scan line buffer}
      j * sz;                          {add offset for this scan line start}
    iscanp[i] := univ_ptr(adr);        {set pointer to where this scan line buffered}
    j := j + 1;                        {advance to next scan line within buffers}
    if j >= inbuf_n then begin         {wrap back to first scan line in buffers}
      j := 0;
      end;
    end;                               {back to set next scan line buffer pointer}

  img_mem_alloc (                      {allocate the output buffer for one scan line}
    img_out,                           {handle to output image file}
    sizeof(img_pixel1_t) * img_out.x_size, {amount of memory to allocate}
    oscan_p);                          {returned pointer to new memory}

  leftx := (img_in.x_size - dx * (img_out.x_size - 1)) / 2.0;
  topy :=  (img_in.y_size - dy * (img_out.y_size - 1)) / 2.0;

  if filter then goto filtering;
{
*   Main loop when filtering is disabled.  In this mode we just use the values
*   from whatever input pixel the ouput pixel center happens to fall within.
}
  scan_p := inbuf_p;                   {we buffer only one input scan in this mode}

  for oy := 0 to img_out.y_size-1 do begin {once for each output scan line}
    y := topy + (oy * dy);             {input coordinate of this output scan line}
    i := trunc(y);                     {input scan first out scan falls within}
    while img_in.next_y <= i do begin  {make sure input scan line is in memory}
      img_read_scan1 (                 {read next input image scan line}
        img_in,                        {handle to input image connection}
        scan_p^,                       {input buffer}
        stat);
      sys_error_abort (stat, 'img', 'read_scan_line', nil, 0);
      end;                             {back until input scan lines is read in}

    for ox := 0 to img_out.x_size-1 do begin {once for each pixel in out scan line}
      x := leftx + (ox * dx);          {input X coordinate of this output pixel}
      j := trunc(x);                   {input pixel this output pixel falls within}
      oscan_p^[ox] := scan_p^[j];      {copy value from selected input pixel}
      end;                             {back to do next output pixel accross}

    img_write_scan1 (img_out, oscan_p^, stat); {write scan line to output image}
    sys_error_abort (stat, 'img', 'write_scan_line', nil, 0);
    end;                               {back to do next output scan line}
  goto leave;                          {clean up and leave}
{
*   Main loop when filtering is enbaled.
}
filtering:
  for oy := 0 to img_out.y_size-1 do begin {once for each output scan line}
    y := topy + (oy * dy);             {input coordinate of this output scan line}
    i := trunc(y);                     {input scan first out scan falls within}
    ty := max(0, min(last_iy, i - irady)); {top input scan we need to look at}
    by := max(0, min(last_iy, i + irady)); {bottom input scan we need to look at}
    while img_in.next_y <= by do begin {read in the neccessary input scan lines}
      img_read_scan1 (                 {read next input image scan line}
        img_in,                        {handle to input image connection}
        iscanp[img_in.next_y]^,        {input buffer}
        stat);
      sys_error_abort (stat, 'img', 'read_scan_line', nil, 0);
      end;                             {back until all needed scan lines are read in}

    for ox := 0 to img_out.x_size-1 do begin {once for each pixel in out scan line}
      x := leftx + (ox * dx);          {input X coordinate of this output pixel}
      j := trunc(x);                   {input pixel this output pixel falls within}
      lx := max(0, j - iradx);         {left input pixel we need to look at}
      rx := min(last_ix, j + iradx);   {right input pixel we need to look at}
      red := 0;                        {init filter convolution accumulators}
      grn := 0;
      blu := 0;
      alpha := 0;
      acc := 0;
      for iy := ty to by do begin      {down the filter rows for this output pixel}
        scan_p := iscanp[iy];          {get pointer to this input scan line}
        for ix := lx to rx do begin    {across this filter row for this output pixel}
          r2 :=                        {make 0.0 to 1.0 filter R**2 at this pixel}
            sqr((ix + 0.5 - x) * radxr) + {contribution from horizontal distance}
            sqr((iy + 0.5 - y) * radyr); {contribution from vertical distance}
          i := trunc(r2 * filt_index_n_k); {make filter coeficient index}
          if i > filt_index_max_k then next; {too far from filter center ?}
          j := filt[i];                {fetch filter coeficient here}
          alpha := scan_p^[ix].alpha * j + alpha; {accumulate weighted contributions}
          red := scan_p^[ix].red * j + red;
          grn := scan_p^[ix].grn * j + grn;
          blu := scan_p^[ix].blu * j + blu;
          acc := j + acc;              {accumulate total filter weight}
          end;                         {back for next pixel across this filter row}
        end;                           {back for next filter row down this out pixel}
      fp := 1.00388 / acc;             {mult factor to normalize filter}
      oscan_p^[ox].alpha := trunc(alpha * fp);
      oscan_p^[ox].red := trunc(red * fp);
      oscan_p^[ox].grn := trunc(grn * fp);
      oscan_p^[ox].blu := trunc(blu * fp);
      end;                             {back to do next output pixel accross}

    img_write_scan1 (img_out, oscan_p^, stat); {write scan line to output image}
    sys_error_abort (stat, 'img', 'write_scan_line', nil, 0);
    end;                               {back to do next output scan line}
{
*   All done writing the output image.  Clean up and leave.
}
leave:                                 {common exit point}
  img_close (img_out, stat);           {close output image}
  img_close (img_in, stat);            {close input image}
  end.
