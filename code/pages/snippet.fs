module FsSnip.Snippet

open FsSnip
open FsSnip.Data
open FsSnip.Utils
open Suave
open Suave.Types
open Suave.Http
open Suave.Http.Applicatives
open Suave.Http.Successful
open Suave.Http.RequestErrors


// -------------------------------------------------------------------------------------------------
// Snippet details and raw view pages
// -------------------------------------------------------------------------------------------------

type Message = { Title : string ; Text : string }
 
type FormattedSnippet =
  { Html : string
    Details : Data.Snippet
    Revision : int }
  
let test input =
  let i = Async.RunSynchronously input
  match i with
  | Some x ->  {x with status= HTTP_404 ;  content = x.content}
  | None -> Response.response HTTP_404 ""
  
  //Response.response HTTP_404 (Array.zeroCreate 0) >>= input
  //NOT_FOUND >>= input
let showInvalidSnippet title text =
  let a = 
    { Title = title ; Text =  text }
    |> DotLiquid.page<Message> "message.html"
  test a

let showSnippet id r =
  let id' = demangleId id
  match Seq.tryFind (fun s -> s.ID = id') publicSnippets with
  | Some snippetInfo -> 
      match Data.loadSnippet id r with
      | Some snippet ->
          let rev = match r with Latest -> snippetInfo.Versions - 1 | Revision r -> r
          { Html = snippet
            Details = Data.snippets |> Seq.find (fun s -> s.ID = id')
            Revision = rev }
          |> DotLiquid.page<FormattedSnippet> "snippet.html"
      | None -> showInvalidSnippet "Can't find the version" (sprintf "Can't find the version you are looking for. Go to <a href='http://fssnip.net/%s'> Latest" id) 
  | None ->
    showInvalidSnippet "Can't find snippet" (sprintf "Can't find snippet <strong>http://fssnip.net/%s</strong> :( " id)

let showRawSnippet id r =
  match Data.loadRawSnippet id r with
  | Some s -> Writers.setMimeType "text/plain" >>= OK s
  | None -> invalidSnippetId id
  
// Web part to be included in the top-level route specification  
let webPart = 
  choose 
    [ pathScan "/%s/%d" (fun (id, r) -> showSnippet id (Revision r))
      pathWithId "/%s" (fun id -> showSnippet id Latest)
      pathScan "/raw/%s/%d" (fun (id, r) -> showRawSnippet id (Revision r))
      pathWithId "/raw/%s" (fun id -> showRawSnippet id Latest) ]
  