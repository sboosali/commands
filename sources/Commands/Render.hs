module Commands.Render where
import Commands.Render.Types


render :: Grammar a -> Dragon
render = undefined -- intercalate " | " . map (intercalate " ") . map _
