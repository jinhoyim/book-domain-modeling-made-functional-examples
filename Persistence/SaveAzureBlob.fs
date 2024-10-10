module Persistence.SaveAzureBlob

open Azure.Storage.Blobs
open System.Text.Json

let connectionString = "...Azure connection string..."
let containerName = "invoices"
let containerClient = BlobContainerClient(connectionString, containerName)
containerClient.CreateIfNotExists()

type PersonDto = {
    PersonId: int
}
let savePersonDtoToBlob personDto =
    let blobId = $"Person%i{personDto.PersonId}"
    let blob = containerClient.GetBlobClient(blobId)
    let json = JsonSerializer.Serialize(personDto)
    blob.Upload(json)
let personDto = { PersonId = 1 }
savePersonDtoToBlob personDto
