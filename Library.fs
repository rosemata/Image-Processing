//
// F# image processing functions.
//
// More details?
//
// Name? School? Date?
//

namespace ImageLibrary

module Operations =
  //
  // all functions must be indented
  //

  //
  // Grayscale:
  //
  // Converts the image into grayscale and returns the 
  // resulting image as a list of lists. Conversion to 
  // grayscale is done by averaging the RGB values for 
  // a pixel, and then replacing them all by that average.
  // So if the RGB values were 25 75 250, the average 
  // would be 116, and then all three RGB values would 
  // become 116 — i.e. 116 116 116.
  //
  // Returns: updated image.
  //

  // creates tuple
  let create_tuple a b c =
    let newTuple = (a, b, c)
    newTuple

  // calculates average
  let avg a b c = 
    let ans = (a+b+c)/3
    let x = create_tuple ans ans ans
    x

  // (0, 0, 0)
  let print_III tuple1 =
    match tuple1 with
      | (a, b, c) -> avg a b c


  // [(0, 0, 0); (100, 0, 0); (0, 0, 0); (255, 0, 255)]
  let rec loop_II list =
    match list with
      |[]      -> []
      |hd::tl  -> print_III hd :: loop_II tl

  // takes [ [(1;2;3);(1;2;3)];[(1;2;3);(1;2;3)]]
  let rec loop_I list =
    match list with
      |[]       -> []
      |hd::tl   -> loop_II hd::loop_I tl

// [[(0, 0, 0); (100, 0, 0); (0, 0, 0); (255, 0, 255)];
//  [(0, 0, 0); (0, 255, 175); (0, 0, 0); (0, 0, 0)];
//  [(0, 0, 0); (0, 0, 0); (0, 15, 175); (0, 0, 0)];
//  [(255, 0, 255); (0, 0, 0); (0, 0, 0); (255, 255, 255)]]


  let rec Grayscale (width:int) 
                    (height:int) 
                    (depth:int) 
                    (image:(int*int*int) list list) = 

    let x = loop_I image
    x



  // check if a,b,c is less than or greater than threshold
  let threshold_helper_IV a b c threshold depth= 
    if (a <= threshold && b <= threshold && c <= threshold) then
      let ans = create_tuple 0 0 0
      ans
    else if (a <= threshold && b <= threshold && c > threshold) then
      let ans = create_tuple 0 0 depth
      ans
    else if (a <= threshold && b > threshold && c <= threshold) then
      let ans = create_tuple 0 depth 0
      ans
    else if (a <= threshold && b > threshold && c > threshold) then
      let ans = create_tuple 0 depth depth
      ans
    else if (a > threshold && b <= threshold && c <= threshold) then
      let ans = create_tuple depth 0 0
      ans
    else if (a > threshold && b <= threshold && c > threshold) then
      let ans = create_tuple depth 0 depth
      ans
    else if (a > threshold && b > threshold && c <= threshold) then
      let ans = create_tuple depth depth 0
      ans
    else
      let ans = create_tuple depth depth depth
      ans

  // takes tuple and separate by element
  let threshold_helper_III tuple1 threshold depth=
    match tuple1 with
      | (a, b, c) -> threshold_helper_IV a b c threshold depth

  // loop tuple by tuple in a list
  let rec threshold_helper_II list threshold depth=
    match list with
      |[]      -> []
      |hd::tl  -> (threshold_helper_III hd threshold depth) :: (threshold_helper_II tl threshold depth)

  // loop list by list
  let rec threshold_helper list threshold depth=
    match list with
      |[]       -> []
      |hd::tl   -> (threshold_helper_II hd threshold depth ) :: (threshold_helper tl threshold depth)

  //
  // Threshold
  //
  // Thresholding increases image separation --- dark values 
  // become darker and light values become lighter. Given a 
  // threshold value in the range 0 < threshold < color depth,
  // each RGB value is compared to see if it's > threshold.
  // If so, that RGB value is replaced by the color depth;
  // if not, that RGB value is replaced with 0. 
  //
  // Example: if threshold is 100 and depth is 255, then given 
  // a pixel (80, 120, 160), the new pixel is (0, 255, 255).
  //
  // Returns: updated image.
  //
  let rec Threshold (width:int) 
                    (height:int)
                    (depth:int)
                    (image:(int*int*int) list list)
                    (threshold:int) = 

    let newList = threshold_helper image threshold depth
    newList


  // reverse list
  let reverseLists list = list |> List.map List.rev

  // takes a tuple and flip them  a,b,c -> c,b,a
  let flip_helper_III tuple1  =
    match tuple1 with
      | (a, b, c) -> create_tuple a b c

  // loop in a list
  let rec flip_helper_II list =
    match list with
      |[]      -> []
      |hd::tl  -> (flip_helper_III hd) :: flip_helper_II tl  
                  
  // loop in a list of list
  let rec flip_helper list=
    match list with
      |[]       -> []
      |hd::tl   -> (flip_helper_II hd) :: (flip_helper tl)

  // comment end

  //
  // FlipHorizontal:
  //
  // Flips an image so that what’s on the left is now on 
  // the right, and what’s on the right is now on the left. 
  // That is, the pixel that is on the far left end of the
  // row ends up on the far right of the row, and the pixel
  // on the far right ends up on the far left. This is 
  // repeated as you move inwards toward the row's center.
  //
  // Returns: updated image.
  //
  let rec FlipHorizontal (width:int)
                         (height:int)
                         (depth:int)
                         (image:(int*int*int) list list) = 

    let newList = flip_helper image
    let reverseList = reverseLists newList
    reverseList


  

  let zoom_III tuple1 factor =
    match tuple1 with
      | (a, b, c) -> create_tuple a b c

  let rec zoom_II list factor =
    match list with
      |[]       -> []
      |hd::tl   -> if (factor = 2) then
                      let x = [(zoom_III hd factor)]
                      let y = x @ x
                      y @ (zoom_II tl factor)
                   
                   else if (factor = 3) then
                      let x = [(zoom_III hd factor)]
                      let y = x @ x @ x
                      y @ (zoom_II tl factor)
                   
                   else if (factor = 4) then
                      let x = [(zoom_III hd factor)]
                      let y = x @ x @ x @ x
                      y @ (zoom_II tl factor)
                   
                   else if (factor = 5) then
                      let x = [(zoom_III hd factor)]
                      let y = x @ x @ x @ x @ x
                      y @ (zoom_II tl factor)

                   else 
                      let x = [(zoom_III hd factor)]
                      x @ (zoom_II tl factor)

  
  let rec zoom_I list factor =
    match list with
      |[]       -> []
      |hd::tl   -> zoom_II hd factor:: zoom_I tl factor




  let rec duple list factor =
    match list with
      |[]       -> []
      |hd::tl   -> if (factor = 2) then
                     let x = List.append [hd] [hd]
                     x @ duple tl factor

                   else if (factor = 3) then
                     let x = List.append [hd] [hd]
                     let y = List.append x [hd]
                     y @ duple tl factor

                   else if (factor = 4) then
                     let x = List.append [hd] [hd]
                     let y = List.append x [hd]
                     let z = List.append y [hd]
                     z @ duple tl factor

                   else if (factor = 5) then
                     let x = List.append [hd] [hd]
                     let y = List.append x [hd]
                     let z = List.append y [hd]
                     let a = List.append z [hd]
                     a @ duple tl factor

                   else
                     list

  //
  // Zoom:
  //
  // Zooms the image by the given zoom factor, which is an 
  // integer 0 < factor < 5. The function uses the nearest 
  // neighbor approach where each pixel P in the original 
  // image is replaced by a factor*factor block of P pixels.
  // For example, if the zoom factor is 4, then each pixel 
  // is replaced by a 4x4 block of 16 identical pixels. 
  // The nearest neighbor algorithm is the simplest zoom 
  // algorithm, but results in jagged images.
  //
  // Returns: updated image.
  //
  let rec Zoom (width:int)
               (height:int)
               (depth:int)
               (image:(int*int*int) list list)
               (factor:int) = 

    let x = zoom_I image factor
    let y = duple x factor
    y


  let removeAt index list =
    list |> List.indexed |> List.filter (fun (i, _) -> i <> index) |> List.map snd

  let rec removal list =
    match list with
      |[]       -> []
      |hd::tl   -> removeAt 0 hd::removal tl

  let rotation image_ =
    let sumList = image_|> List.map (fun x -> List.head x)
    let reverseLists1 = sumList |> List.rev
    reverseLists1

  let rec helping list  =
    match list with
      |[]       -> []
      |hd::tl   -> let x = removal list    
                   if (List.isEmpty (List.head x) = true) then
                     rotation list :: helping []
                   else
                     rotation list :: helping x

  

  //
  // RotateRight90:
  //
  // Rotates the image to the right 90 degrees.
  //
  // Returns: updated image.
  //
  let rec RotateRight90 (width:int)
                        (height:int)
                        (depth:int)
                        (image:(int*int*int) list list) = 

    
    // loop through every list
    // takes the first element in every list
    // append it to a new list
    // then remove the first element in the new list
    // repeat

    let m = []
    let x  = helping image
    x