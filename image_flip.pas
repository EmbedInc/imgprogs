{   Program IMAGE_FLIP <options>
*
*   This program can perform manipulations on an image, such as rotating and
*   mirror flipping it.  The command line options are:
*
*   <input file name>
*
*   -IN <input file name>
*
*   <output file name>
*
*   -OUT <output file name>
*
*   -FLIPLR
*
*      Flip the image left-to-right.  This produces a mirror image.
*
*   -FLIPUD
*
*      Flip the image upside down.  Note that this is different from rotating
*      the image upside down.  -FLIPUD can be thought of as a 180 degree rotation
*      follwed by a left-to-right mirror flip.
*
*   -ROTR
*
*      Rotate the image 1/4 turn (90 degrees) to the right.
*
*   -ROTL
*
*      -FLIPLR, -FLIPUD, -ROTR
*
*   -ROTUD
*
*      -FLIPLR, -FLIPUD
}
program image_flip;
%include 'sys.ins.pas';
%include 'util.ins.pas';
%include 'string.ins.pas';
%include 'file.ins.pas';
%include 'img.ins.pas';

const
  max_image_width = 16384;             {max pixel image width this program can handle}
  max_image_height = 16384;            {max image height this program can handle}
  runs_per_buffer = 52428;             {number of runs in one buffer (about .5 Mb)}
  max_msg_parm = 4;                    {max parameters we can pass to a message}

  max_x = max_image_width-1;           {max allowable X pixel coordinate}
  max_y = max_image_height-1;          {max allowable Y pixel coordinate}

type
  run_t = record                       {descriptor for one run in buffers}
    n: 0..65535;                       {number of pixels in run - 1}
    pix: img_pixel2_t;                 {pixels value for this run}
    end;
  run_p_t = ^run_t;

  buf_p_t = ^buf_t;
  buf_t = record                       {template for one dynamically allocated buffer}
    next_p: buf_p_t;                   {points to next buffer in chain}
    run: array[1..runs_per_buffer] of run_t; {all the runs for this buffer}
    end;

  pixp_t = record                      {descriptor that points to one pixel in a run}
    p: run_p_t;                        {pointer to the run data structure}
    pnum: sys_int_machine_t;           {number of pixel into run - 1}
    buf_p: buf_p_t;                    {pointer to buffer holding this run}
    bi: sys_int_adr_t;                 {buffer index for this run}
    end;

var
  fnam_in, fnam_out:                   {input and output file names}
    %include '(cog)lib/string_treename.ins.pas';
  iname_set: boolean;                  {TRUE if the input file name already set}
  oname_set: boolean;                  {TRUE if the output file name already set}
  rotr: boolean;                       {TRUE on -ROTL command line option}
  fliplr: boolean;                     {TRUE on -FLIPLR command line option}
  flipud: boolean;                     {TRUE on -FLIPUD command line option}

  img_fmt:                             {image output file format string}
    %include '(cog)lib/string_treename.ins.pas';
  img_in: img_conn_t;                  {handle to input image file}
  img_out: img_conn_t;                 {handle to output image file}
  scan:                                {one scan line buffer}
    array[0..max_x] of img_pixel2_t;
  x, y: sys_int_machine_t;             {pixel coordinate}
  ix: sys_int_machine_t;               {input pixel X coordinate}
  ix_start: sys_int_machine_t;         {scan line start value for IX}
  ix_inc: sys_int_machine_t;           {IX increment to get to next pixel}
  sy: sys_int_machine_t;               {Y index for saved scan starts}
  aspect: real;                        {aspect ratio of output image}
  vpix:                                {array of curr vertical scan pixel pointers}
    array[0..max_y] of pixp_t;
  vx: sys_int_machine_t;               {used as index for VPIX}
  run_val: img_pixel2_t;               {pixel value for the current run}
  run_n: sys_int_machine_t;            {number of pixels in the current run}
  run_i: sys_int_machine_t;            {buffer array index for current run}
  buf_p: buf_p_t;                      {pointer to current buffer of runs}

  opt:                                 {upcased command line option}
    %include '(cog)lib/string_treename.ins.pas';
  parm:                                {command line option parameter}
    %include '(cog)lib/string_treename.ins.pas';
  pick: sys_int_machine_t;             {number of token picked from list}

  msg_parm:                            {parameter references for messages}
    array[1..max_msg_parm] of sys_parm_msg_t;
  stat: sys_err_t;                     {completion status code}

label
  next_opt, err_parm, parm_bad, done_opts;
{
********************************************************************
*
*   Local subroutine WRITE_RUN
*
*   Write the current run into the current buffer slot.  Reset the run
*   size to zero, and set up the next buffer slot.
}
procedure write_run;

var
  bp: buf_p_t;                         {pointer to new runs buffer}

begin
  if run_n <= 0 then return;           {nothing to write out ?}

  buf_p^.run[run_i].n := run_n - 1;    {write current run to buffer}
  buf_p^.run[run_i].pix := run_val;

  run_n := 0;                          {reset to no pixels in current run}
  run_i := run_i + 1;                  {make index of where next run will go}

  if run_i > runs_per_buffer then begin {we just filled current buffer ?}
    sys_mem_alloc (sizeof(buf_p^), bp); {allocate new buffer}
    sys_mem_error (bp, '', '', nil, 0); {complain if not got new memory}
    buf_p^.next_p := bp;               {link new buffer right after current buffer}
    buf_p := bp;                       {make new buffer the current buffer}
    run_i := 1;                        {index of where next run will go}
    end;
  end;
{
********************************************************************
*
*   Start of main routine.
}
begin
  string_cmline_init;                  {init for reading the command line}
{
*   Initialize our state before reading the command line options.
}
  iname_set := false;
  oname_set := false;
  rotr := false;
  fliplr := false;
  flipud := false;
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
    '-IN -OUT -FLIPLR -FLIPUD -ROTR -ROTL -ROTUD',
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
*   -FLIPLR
}
3: begin
  fliplr := not fliplr;
  end;
{
*   -FLIPUD
}
4: begin
  flipud := not flipud;
  end;
{
*   -ROTR
}
5: begin
  rotr := not rotr;
  end;
{
*   -ROTL
}
6: begin
  fliplr := not fliplr;
  flipud := not flipud;
  rotr := not rotr;
  end;
{
*   -ROTUD
}
7: begin
  fliplr := not fliplr;
  flipud := not flipud;
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
*   Open the input image file.
}
  if not iname_set then begin          {no input file name specified ?}
    sys_message_bomb ('img', 'input_fnam_missing', nil, 0);
    end;

  img_open_read_img (fnam_in, img_in, stat); {open input image for read}
  sys_msg_parm_vstr (msg_parm[1], fnam_in);
  sys_error_abort (stat, 'img', 'open_read', msg_parm, 1);

  if not oname_set then begin          {no explicit output file name given ?}
    string_copy (img_in.gnam, fnam_out); {default is generic leaf name of input file}
    end;
{
*   Look at the input file handle to decide how to open the output file,
*   then open the output file.
}
  string_list_pos_last (img_in.comm);  {move to last comment line}
  string_list_line_add (img_in.comm);  {create comment line for date}
  sys_date_time1 (img_in.comm.str_p^); {init our comment line with date/time string}
  string_appends (img_in.comm.str_p^, '  IMAGE_FLIP'(0));
  if rotr then begin
    string_appends (img_in.comm.str_p^, ' -ROTR'(0));
    end;
  if fliplr then begin
    string_appends (img_in.comm.str_p^, ' -FLIPLR'(0));
    end;
  if flipud then begin
    string_appends (img_in.comm.str_p^, ' -FLIPUD'(0));
    end;

  string_list_line_add (img_in.comm);  {create comment line for file name}
  string_appends (img_in.comm.str_p^, '  from file '(0));
  string_append (img_in.comm.str_p^, img_in.tnam);

  img_conn_fmt (img_in, img_fmt);      {specify same bits/pixel as input file}

  if rotr                              {set output image size in pixels}
    then begin                         {image is being rotated 1/4 turn}
      x := img_in.y_size;
      y := img_in.x_size;
      aspect := 1.0 / img_in.aspect;
      end
    else begin                         {image is not being rotated}
      x := img_in.x_size;
      y := img_in.y_size;
      aspect := img_in.aspect;
      end
    ;

  img_open_write_img (                 {open output image for write}
    fnam_out,                          {output file name}
    aspect,                            {aspect ratio of output image}
    x, y,                              {size of output image in pixels}
    '',                                {select no explicit file type}
    img_fmt,                           {output file special requests}
    img_in.comm,                       {comments list}
    img_out,                           {new image connection handle}
    stat);
  sys_msg_parm_vstr (msg_parm[1], fnam_out);
  sys_error_abort (stat, 'img', 'open_write', msg_parm, 1);
{
*   Done opening output image file.
}
  if img_in.x_size > max_image_width then begin
    sys_message_bomb ('img', 'image_too_wide', nil, 0);
    end;

  if img_in.y_size > max_image_height then begin
    sys_message_bomb ('img', 'image_too_tall', nil, 0);
    end;
{
*   Read in the input file.  The input file will be saved in terms of horizontal
*   runs to reduce memory requirements.  The index into the start of each scan line
*   will be saved so that the run length info can be accessed for writing the output
*   file.
}
  sys_mem_alloc (sizeof(buf_p^), buf_p); {allocate first runs buffer}
  sys_mem_error (buf_p, '', '', nil, 0); {complain if not got new memory}
  run_i := 1;                          {buffer index where next run will go}
  run_n := 0;                          {init number of pixels in current run}

  if fliplr
    then begin                         {flip input image left to right}
      ix_start := img_in.x_size - 1;   {start at right end of input scan lines}
      ix_inc := -1;                    {next pixel is one to the left}
      end
    else begin                         {preserve image left/right}
      ix_start := 0;                   {start at left end of input scan line}
      ix_inc := 1;                     {next pixel is one to the right}
      end
    ;

  if flipud
    then begin                         {scan lines will be stored upside down}
      sy := img_in.y_size - 1;         {start writing at the end of the array}
      end
    else begin                         {scan lines will be stored top to bottom}
      sy := 0;                         {start writing at the start of the array}
      end
    ;

  for y := 0 to img_in.y_size-1 do begin {once for each scan line in input file}
    img_read_scan2 (img_in, scan, stat); {read next scan line from input image}
    sys_error_abort (stat, 'img', 'read_scan_line', nil, 0);

    ix := ix_start;                    {init input index for this scan line}
    for x := 0 to img_in.x_size-1 do begin {once for each pixel in scan line}
      if                               {need to end previous run ?}
          (scan[ix].alpha <> run_val.alpha) or
          (scan[ix].red <> run_val.red) or
          (scan[ix].grn <> run_val.grn) or
          (scan[ix].blu <> run_val.blu)
          then begin
        write_run;                     {write old run to buffer}
        run_val := scan[ix];           {set pixel value for new run}
        end;
      if x = 0 then begin              {this is first pixel in saved scan line ?}
        vpix[sy].p := addr(buf_p^.run[run_i]); {save address of where run will go}
        vpix[sy].pnum := run_n;        {number of pixels into run for scan start}
        vpix[sy].buf_p := buf_p;       {save pointer to buffer holding this run}
        vpix[sy].bi := run_i;          {save buffer index for this run}
        end;
      run_n := run_n + 1;              {one more pixel in current run}
      if run_n >= 65535 then begin     {current run will fill buffer run ?}
        write_run;                     {write current run to buffer}
        end;
      ix := ix + ix_inc;               {advance to next input scan line index}
      end;                             {back and process next pixel in scan line}

    if flipud                          {advance to next VPIX array index}
      then sy := sy - 1
      else sy := sy + 1;
    end;                               {back and process next scan line down}
  write_run;                           {write out any remaining partial run}
  img_close (img_in, stat);            {close input image file}
  sys_msg_parm_vstr (msg_parm[1], fnam_in);
  sys_error_abort (stat, 'img', 'close', msg_parm, 1);
{
*   The input file has been read in and stored as a list of horizontal runs.  The
*   location for the start pixel of each scan line was saved in the array VPIX.
*   The FLIPLR and FLIPUD flip flags have already been taken into account by
*   the left/right top/bottom order the data was saved in.
*
*   Now write the output image.  The ROTR flag is taken into account here.
}
  if rotr
{
*   The image is being rotated 1/4 turn to the right.  Each scan line in the
*   output file is made of the pixels pointed to by VPIX.  Each pixel location
*   in VPIX is advanced by one every output file scan line.
}
    then begin

  for y := 0 to img_out.y_size-1 do begin {once for each output file scan line}
    for x := 0 to img_out.x_size-1 do begin {once for each pixel in output line}
      vx := img_out.x_size-1-x;        {make VPIX index}
      if vpix[vx].pnum > vpix[vx].p^.n then begin {exhausted this run ?}
        vpix[vx].bi := vpix[vx].bi+1;  {advance buffer index to next run}
        if vpix[vx].bi > runs_per_buffer then begin {exhausted current buffer ?}
          vpix[vx].buf_p := vpix[vx].buf_p^.next_p; {point to next buffer in chain}
          vpix[vx].bi := 1;            {reset index to first run in new buffer}
          end;
        vpix[vx].p :=                  {make address of new run}
          addr(vpix[vx].buf_p^.run[vpix[vx].bi]);
        vpix[vx].pnum := 0;            {init number of pixels used from new run}
        end;                           {this VPIX entry definately points to pixel}
      scan[x].alpha := vpix[vx].p^.pix.alpha; {get value for this pixel}
      scan[x].red := vpix[vx].p^.pix.red;
      scan[x].grn := vpix[vx].p^.pix.grn;
      scan[x].blu := vpix[vx].p^.pix.blu;
      vpix[vx].pnum := vpix[vx].pnum+1; {point to next pixel from this input scan}
      end;                             {back and get next output pixel accross}
    img_write_scan2 (img_out, scan, stat);
    sys_error_abort (stat, 'img', 'write_scan_line', nil, 0);
    end;                               {back and do next output scan line down}

      end                              {end of rotating right case}
{
*   The image is not being rotated.
}
    else begin

  for y := 0 to img_out.y_size-1 do begin {once for each output file scan line}
    for x := 0 to img_out.x_size-1 do begin {once for each pixel in output line}
      if vpix[y].pnum > vpix[y].p^.n then begin {exhausted this run ?}
        vpix[y].bi := vpix[y].bi+1;    {advance buffer index to next run}
        if vpix[y].bi > runs_per_buffer then begin {exhausted current buffer ?}
          vpix[y].buf_p := vpix[y].buf_p^.next_p; {point to next buffer in chain}
          vpix[y].bi := 1;             {reset index to first run in new buffer}
          end;
        vpix[y].p :=                   {make address of new run}
          addr(vpix[y].buf_p^.run[vpix[y].bi]);
        vpix[y].pnum := 0;             {init number of pixels used from new run}
        end;                           {this VPIX entry definately points to pixel}
      scan[x].alpha := vpix[y].p^.pix.alpha; {get value for this pixel}
      scan[x].red := vpix[y].p^.pix.red;
      scan[x].grn := vpix[y].p^.pix.grn;
      scan[x].blu := vpix[y].p^.pix.blu;
      vpix[y].pnum := vpix[y].pnum + 1; {point to next pixel from this input scan}
      end;                             {back and get next output pixel accross}
    img_write_scan2 (img_out, scan, stat);
    sys_error_abort (stat, 'img', 'write_scan_line', nil, 0);
    end;                               {back and do next output scan line down}

      end                              {end of not rotating case}
    ;                                  {end of rotating yes/no cases}

  img_close (img_out, stat);           {close output image file}
  sys_error_print (stat, 'img', 'close', msg_parm, 1);
  end.
