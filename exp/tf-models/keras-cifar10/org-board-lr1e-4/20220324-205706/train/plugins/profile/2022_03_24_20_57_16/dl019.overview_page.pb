�%  *�p=
��U@���SAhA2�
xIterator::Model::MaxIntraOpParallelism::Prefetch::MapAndBatch::Prefetch::ParallelMapV2::Shuffle::Prefetch::ParallelMapV2�����nX@!�ڃߛJ@)�����nX@1�ڃߛJ@:Preprocessing2�
VIterator::Model::MaxIntraOpParallelism::Prefetch::MapAndBatch::Prefetch::ParallelMapV2�C�+^Q@!WZc�B@)C�+^Q@1WZc�B@:Preprocessing2�
�Iterator::Model::MaxIntraOpParallelism::Prefetch::MapAndBatch::Prefetch::ParallelMapV2::Shuffle::Prefetch::ParallelMapV2::MemoryCacheImpl::ParallelMapV2	�p��)@!gK0*@)	�p��)@1gK0*@:Preprocessing2�
_Iterator::Model::MaxIntraOpParallelism::Prefetch::MapAndBatch::Prefetch::ParallelMapV2::Shuffle�?�')@!l~��U��?)J�O�cF�?1b�_� ��?:Preprocessing2
GIterator::Model::MaxIntraOpParallelism::Prefetch::MapAndBatch::Prefetch�g�W���?!(�P!�?)g�W���?1(�P!�?:Preprocessing2�
iIterator::Model::MaxIntraOpParallelism::Prefetch::MapAndBatch::Prefetch::ParallelMapV2::Shuffle::Prefetch��L�:�/�?!�M�+��?)�L�:�/�?1�M�+��?:Preprocessing2�
�Iterator::Model::MaxIntraOpParallelism::Prefetch::MapAndBatch::Prefetch::ParallelMapV2::Shuffle::Prefetch::ParallelMapV2::MemoryCacheImpl::ParallelMapV2::AssertCardinality��� �r�?!���u*��?)X:�%ȼ?1wG� <X�?:Preprocessing2�
�Iterator::Model::MaxIntraOpParallelism::Prefetch::MapAndBatch::Prefetch::ParallelMapV2::Shuffle::Prefetch::ParallelMapV2::MemoryCacheImpl::ParallelMapV2::AssertCardinality::ParallelInterleaveV4��i�:�?!�����?)��i�:�?1�����?:Preprocessing2�
�Iterator::Model::MaxIntraOpParallelism::Prefetch::MapAndBatch::Prefetch::ParallelMapV2::Shuffle::Prefetch::ParallelMapV2::MemoryCacheImplZ�b+h*@!˳���d@)g(�x�ߺ?1��W�(D�?:Preprocessing2�
�Iterator::Model::MaxIntraOpParallelism::Prefetch::MapAndBatch::Prefetch::ParallelMapV2::Shuffle::Prefetch::ParallelMapV2::MemoryCacheImpl::ParallelMapV2::AssertCardinality::ParallelInterleaveV4[0]::FlatMap[0]::TFRecord�����Q�?!*��ʇN�?)�����Q�?1*��ʇN�?:Advanced file read2�
�Iterator::Model::MaxIntraOpParallelism::Prefetch::MapAndBatch::Prefetch::ParallelMapV2::Shuffle::Prefetch::ParallelMapV2::MemoryCache���E.*@!��Ɍ�@)T���ݫ?1���2�X�?:Preprocessing2�
�Iterator::Model::MaxIntraOpParallelism::Prefetch::MapAndBatch::Prefetch::ParallelMapV2::Shuffle::Prefetch::ParallelMapV2::MemoryCacheImpl::ParallelMapV2::AssertCardinality::ParallelInterleaveV4[0]::FlatMapu�V�?!� �!��?)� ��ԕ�?1
~v��?:Preprocessing2]
&Iterator::Model::MaxIntraOpParallelism-z��y�?!ަ�GR��?)��25	ސ?1=|ӳ�^�?:Preprocessing2g
0Iterator::Model::MaxIntraOpParallelism::Prefetch�$��7�?!����9�}?)�$��7�?1����9�}?:Preprocessing2t
=Iterator::Model::MaxIntraOpParallelism::Prefetch::MapAndBatchj��늉?!�9~+�{?)j��늉?1�9~+�{?:Preprocessing2F
Iterator::Model3�z���?!@�z76:�?)���SVs?1�g}e?:Preprocessing:�
]Enqueuing data: you may want to combine small input data chunks into fewer but larger chunks.
�Data preprocessing: you may increase num_parallel_calls in <a href="https://www.tensorflow.org/api_docs/python/tf/data/Dataset#map" target="_blank">Dataset map()</a> or preprocess the data OFFLINE.
�Reading data from files in advance: you may tune parameters in the following tf.data API (<a href="https://www.tensorflow.org/api_docs/python/tf/data/Dataset#prefetch" target="_blank">prefetch size</a>, <a href="https://www.tensorflow.org/api_docs/python/tf/data/Dataset#interleave" target="_blank">interleave cycle_length</a>, <a href="https://www.tensorflow.org/api_docs/python/tf/data/TFRecordDataset#class_tfrecorddataset" target="_blank">reader buffer_size</a>)
�Reading data from files on demand: you should read data IN ADVANCE using the following tf.data API (<a href="https://www.tensorflow.org/api_docs/python/tf/data/Dataset#prefetch" target="_blank">prefetch</a>, <a href="https://www.tensorflow.org/api_docs/python/tf/data/Dataset#interleave" target="_blank">interleave</a>, <a href="https://www.tensorflow.org/api_docs/python/tf/data/TFRecordDataset#class_tfrecorddataset" target="_blank">reader buffer</a>)
�Other data reading or processing: you may consider using the <a href="https://www.tensorflow.org/programmers_guide/datasets" target="_blank">tf.data API</a> (if you are not using it now)�
:type.googleapis.com/tensorflow.profiler.BottleneckAnalysisk
unknownTNo step time measured. Therefore we cannot tell where the performance bottleneck is.no*noZno#You may skip the rest of this page.BZ
@type.googleapis.com/tensorflow.profiler.GenericStepTimeBreakdown
  " * 2 : B J R Z b JGPUb��No step marker observed and hence the step time is unknown. This may happen if (1) training steps are not instrumented (e.g., if you are not using Keras) or (2) the profiling duration is shorter than the step time. For (1), you need to add step instrumentation; for (2), you may try to profile longer.Y      Y@q����?"�
unknownTNo step time measured. Therefore we cannot tell where the performance bottleneck is.b
`input_pipeline_analyzer (especially Section 3 for the breakdown of input operations on the Host)Q
Otf_data_bottleneck_analysis (find the bottleneck in the tf.data input pipeline)m
ktrace_viewer (look at the activities on the timeline of each Host Thread near the bottom of the trace view)"O
Mtensorflow_stats (identify the time-consuming operations executed on the GPU)"U
Strace_viewer (look at the activities on the timeline of each GPU in the trace view)*�
�<a href="https://www.tensorflow.org/guide/data_performance_analysis" target="_blank">Analyze tf.data performance with the TF Profiler</a>*y
w<a href="https://www.tensorflow.org/guide/data_performance" target="_blank">Better performance with the tf.data API</a>2M
=type.googleapis.com/tensorflow.profiler.GenericRecommendation
nono2no:
Refer to the TF2 Profiler FAQ2"Nvidia GPU (Pascal)(: B��No step marker observed and hence the step time is unknown. This may happen if (1) training steps are not instrumented (e.g., if you are not using Keras) or (2) the profiling duration is shorter than the step time. For (1), you need to add step instrumentation; for (2), you may try to profile longer.@dl019: Failed to load libcupti (is it installed and accessible?)