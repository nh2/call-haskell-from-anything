#!/usr/bin/env php
<?php

$error_messages = [];
if (phpversion() < 7)
{
  $error_messages[] = "PHP version 7 or higher is required in for the MFFI PHP extension";
}
if (!is_dir('vendor/rybakit/msgpack'))
{
  $error_messages[] = "MessagePack library not not found, please install with: composer require rybakit/msgpack";
}

if (!extension_loaded('mffi'))
{

  $error_messages[] = "MFFI PHP extension not not found, please install from: https://github.com/mgdm/MFFI \n" .
                      "Or use the php-ffi-setup.sh (tested on OS X)";
}

if (count($error_messages))
{

  printf("Can't run test.php due to missing dependencies:\n");
  foreach ($error_messages as $msg)
  {
    printf("\n - %s\n", $msg);
  }
  die();
}

require(__DIR__.'/vendor/autoload.php');
use MessagePack\Packer;
use MessagePack\BufferUnpacker;

// find call-haskell-from-anything.so
$iterator = new RegexIterator(
  new RecursiveIteratorIterator(new RecursiveDirectoryIterator('.')),
  '/bin\/call-haskell-from-anything\.so/',
  RegexIterator::GET_MATCH
);
$so_files = iterator_to_array($iterator);

if (count($so_files) !== 1)
{
  print_r($so_files);
  die("Could not find call-haskell-from-anything.so");
}

$haskell = array_keys($so_files)[0];


$packer = new Packer();
$unpacker = new BufferUnpacker();



// Attach to Haskell functions through MFFI
$handle = new MFFI\Library($haskell);
$f1_t_export = $handle->bind('f1_t_export', [MFFI\Type::TYPE_STRING], MFFI\Type::TYPE_STRING);
$hs_init = $handle->bind('hs_init', [MFFI\Type::TYPE_INT, MFFI\Type::TYPE_INT], MFFI\Type::TYPE_VOID);
$hs_exit = $handle->bind('hs_exit', [], MFFI\Type::TYPE_VOID);

$hs_init( 0, 0);


# Call function
$packed_input = $packer->pack([1, 2.23]);
$packed_output = $f1_t_export( $packed_input );
$unpacker->reset($packed_output);
$result = $unpacker->unpack();

printf("Haskell said: %s\n", $result);


# Shortcut to automatically pack/unpack args and return values
$make_msgpack_fun = function ($fun_sym) use ($handle, $packer, $unpacker)
{
  $func = $handle->bind($fun_sym, [MFFI\Type::TYPE_STRING], MFFI\Type::TYPE_STRING);
  return function($args) use ($packer, $unpacker, $func) {
    $packed_args = $packer->pack($args);
    $packed_result = $func($packed_args);
    $unpacker->reset($packed_result);
    return $unpacker->unpack();
  };
};

# Now we can attach to functions and automatically pack/unpack data
$fib = $make_msgpack_fun('fib_export');
// TODO: find out why fib needs an array argument in PHP, unlike the Ruby and Python examples
printf("Haskell fib: %s\n", $fib([13]));

echo PHP_EOL;
// benchmark Haskell
printf("Haskell results:\n");
$sum = 0;
$start = microtime(true);
for ($i = 0; $i <= 100000; $i++)
{
  $sum += $fib([15]);
}
$end = microtime(true);

printf("Sum: %s\n", $sum);
printf("Time: %ss", round($end- $start,3));

echo PHP_EOL;
echo PHP_EOL;
// benchmark PHP
printf("PHP results:\n");
function fib_php($n)
{
  if ($n === 0 || $n === 1)
    return 1;

  return fib_php($n-1) + fib_php($n-2);
}

$sum = 0;
$start = microtime(true);
for ($i = 0; $i <= 100000; $i++)
{
  $sum += fib_php(15);
}
$end = microtime(true);

printf("Sum: %s\n", $sum);
printf("Time: %ss", round($end-$start,3));

echo PHP_EOL;
$hs_exit();

