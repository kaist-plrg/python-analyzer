  *��S�%b�@X9�<%�@2�
VIterator::Model::MaxIntraOpParallelism::Prefetch::MapAndBatch::Prefetch::ParallelMapV2�衶��8@!/��O<@)衶��8@1/��O<@:Preprocessing2�
�Iterator::Model::MaxIntraOpParallelism::Prefetch::MapAndBatch::Prefetch::ParallelMapV2::Shuffle::Prefetch::ParallelMapV2::MemoryCacheImpl::ParallelMapV2::AssertCardinality::ParallelInterleaveV4�衶C%@!9a=�O�(@)衶C%@19a=�O�(@:Preprocessing2�
xIterator::Model::MaxIntraOpParallelism::Prefetch::MapAndBatch::Prefetch::ParallelMapV2::Shuffle::Prefetch::ParallelMapV2�A�c�]�@!�Oh� @)A�c�]�@1�Oh� @:Preprocessing2�
_Iterator::Model::MaxIntraOpParallelism::Prefetch::MapAndBatch::Prefetch::ParallelMapV2::Shuffle���g�l*@!����y.@);s	��@1㎇��@:Preprocessing2�
iIterator::Model::MaxIntraOpParallelism::Prefetch::MapAndBatch::Prefetch::ParallelMapV2::Shuffle::Prefetch�����@!�5R��@)����@1�5R��@:Preprocessing2�
�Iterator::Model::MaxIntraOpParallelism::Prefetch::MapAndBatch::Prefetch::ParallelMapV2::Shuffle::Prefetch::ParallelMapV2::MemoryCacheImpl::ParallelMapV2�M��~�@!�nSP@)M��~�@1�nSP@:Preprocessing2�
�Iterator::Model::MaxIntraOpParallelism::Prefetch::MapAndBatch::Prefetch::ParallelMapV2::Shuffle::Prefetch::ParallelMapV2::MemoryCacheImpl��S����$@!d�<o>$(@)��v��@1<�%_p�@:Preprocessing2
GIterator::Model::MaxIntraOpParallelism::Prefetch::MapAndBatch::Prefetch��j�1@!���e,J@)�j�1@1���e,J@:Preprocessing2�
�Iterator::Model::MaxIntraOpParallelism::Prefetch::MapAndBatch::Prefetch::ParallelMapV2::Shuffle::Prefetch::ParallelMapV2::MemoryCacheImpl::ParallelMapV2::AssertCardinality�\>���.@!DM��i�1@)~��!@1�rf�@:Preprocessing2�
�Iterator::Model::MaxIntraOpParallelism::Prefetch::MapAndBatch::Prefetch::ParallelMapV2::Shuffle::Prefetch::ParallelMapV2::MemoryCacheImpl::ParallelMapV2::AssertCardinality::ParallelInterleaveV4[0]::FlatMap[0]::TFRecord��U����@!��Y@)�U����@1��Y@:Advanced file read2�
�Iterator::Model::MaxIntraOpParallelism::Prefetch::MapAndBatch::Prefetch::ParallelMapV2::Shuffle::Prefetch::ParallelMapV2::MemoryCacheImpl::ParallelMapV2::AssertCardinality::ParallelInterleaveV4[0]::FlatMap���(�[:@!4����"@)��ܴ�@1d�Ҕ�@:Preprocessing2�
�Iterator::Model::MaxIntraOpParallelism::Prefetch::MapAndBatch::Prefetch::ParallelMapV2::Shuffle::Prefetch::ParallelMapV2::MemoryCache����=[+@!�^]��/@)�K6l�	@1W-w���@:Preprocessing2]
&Iterator::Model::MaxIntraOpParallelism���	.V�?!�q��!t�?)_~�Ɍ��?1�U��?:Preprocessing2t
=Iterator::Model::MaxIntraOpParallelism::Prefetch::MapAndBatch�1���?!�6&:��?)�1���?1�6&:��?:Preprocessing2g
0Iterator::Model::MaxIntraOpParallelism::Prefetch��UJ���?!���ܕ?)��UJ���?1���ܕ?:Preprocessing2F
Iterator::ModelE���JY�?!�B7o2Ʃ?)��W�p?1o�n
��r?:Preprocessing:�
]Enqueuing data: you may want to combine small input data chunks into fewer but larger chunks.
�Data preprocessing: you may increase num_parallel_calls in <a href="https://www.tensorflow.org/api_docs/python/tf/data/Dataset#map" target="_blank">Dataset map()</a> or preprocess the data OFFLINE.
�Reading data from files in advance: you may tune parameters in the following tf.data API (<a href="https://www.tensorflow.org/api_docs/python/tf/data/Dataset#prefetch" target="_blank">prefetch size</a>, <a href="https://www.tensorflow.org/api_docs/python/tf/data/Dataset#interleave" target="_blank">interleave cycle_length</a>, <a href="https://www.tensorflow.org/api_docs/python/tf/data/TFRecordDataset#class_tfrecorddataset" target="_blank">reader buffer_size</a>)
�Reading data from files on demand: you should read data IN ADVANCE using the following tf.data API (<a href="https://www.tensorflow.org/api_docs/python/tf/data/Dataset#prefetch" target="_blank">prefetch</a>, <a href="https://www.tensorflow.org/api_docs/python/tf/data/Dataset#interleave" target="_blank">interleave</a>, <a href="https://www.tensorflow.org/api_docs/python/tf/data/TFRecordDataset#class_tfrecorddataset" target="_blank">reader buffer</a>)
�Other data reading or processing: you may consider using the <a href="https://www.tensorflow.org/programmers_guide/datasets" target="_blank">tf.data API</a> (if you are not using it now)�
:type.googleapis.com/tensorflow.profiler.BottleneckAnalysisk
unknownTNo step time measured. Therefore we cannot tell where the performance bottleneck is.no*noZno#You may skip the rest of this page.BZ
@type.googleapis.com/tensorflow.profiler.GenericStepTimeBreakdown
  " * 2 : B J R Z b JGPUb��No step marker observed and hence the step time is unknown. This may happen if (1) training steps are not instrumented (e.g., if you are not using Keras) or (2) the profiling duration is shorter than the step time. For (1), you need to add step instrumentation; for (2), you may try to profile longer.