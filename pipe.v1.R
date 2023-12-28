# Some functions to do stuff
import_data <- function(file_name)
{
  data.matrix(read.table(file_name, 
                         row.names=1, 
                         header=TRUE, 
                         sep="\t", 
                         comment.char="", 
                         quote="", check.names=FALSE))
}


library(parallel)
# get the number of cores
getNumberOfCores <- function() {
  require(parallel)
  num_cores <- parallel::detectCores()
  return(num_cores)
}
# Call the function to get the number of cores
num_cores <- getNumberOfCores()
print(num_cores)  # Output the number of cores




library(systemPipeR)  # For pipeline construction in R

# Define the workflow steps
fastqToVCF <- systemPipeR::pipeline(
  # Step 1: Quality Control and Trimming
  systemPipeR::rule(
    targets = "cleaned_fastq.gz",
    input = "raw_fastq.gz",
    cmd = "trim_galore --quality 20 --gzip --length 36 -o output_folder raw_fastq.gz"
  ),
  
  # Step 2: Alignment
  systemPipeR::rule(
    targets = "aligned_reads.bam",
    input = "cleaned_fastq.gz",
    cmd = "bwa mem -t 4 reference_genome.fa cleaned_fastq.gz | samtools view -bS - > aligned_reads.bam"
  ),
  
  # Step 3: Variant Calling
  systemPipeR::rule(
    targets = "variants.vcf",
    input = "aligned_reads.bam",
    cmd = "gatk HaplotypeCaller -I aligned_reads.bam -O variants.vcf"
  ),
  
  # Step 4: Variant Annotation
  systemPipeR::rule(
    targets = "annotated_variants.vcf",
    input = "variants.vcf",
    cmd = "vep -i variants.vcf -o annotated_variants.vcf --species human --assembly GRCh38"
  )
)

# Run the pipeline
run(fastqToVCF, jobs = 4)  # Specify the number of jobs/cores













library(dplyr)  # For data manipulation
library(magrittr)  # For piping

# Example raw NGS data (replace this with your actual data)
raw_ngs_data <- data.frame(
  Sample = c("Sample1", "Sample2", "Sample3"),
  ReadCount = c(10000, 12000, 9000),
  # Other columns representing NGS data
)

# Simulated data preprocessing pipeline
processed_data <- raw_ngs_data %>%
  filter(ReadCount > 8000) %>%  # Filter based on ReadCount threshold
  mutate(NormalizedCount = ReadCount / max(ReadCount)) %>%  # Add a normalized count column
  group_by(Sample) %>%  # Group by Sample
  summarize(AvgReadCount = mean(ReadCount)) %>%  # Calculate average ReadCount per Sample
  arrange(desc(AvgReadCount))  # Arrange in descending order of average ReadCount

# View the processed data
processed_data